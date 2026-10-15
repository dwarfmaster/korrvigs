module Korrvigs.AddressBook.DAV where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Data.VCard
import Korrvigs.Entry
import Korrvigs.Entry.New (neMtdt, neParents)
import Korrvigs.Metadata
import Korrvigs.Metadata.Contact
import Korrvigs.Monad
import Korrvigs.Monad.Metadata
import Korrvigs.Note.New
import Korrvigs.Utils
import qualified Korrvigs.Utils.DAV.Card as DAV
import Korrvigs.Utils.DAV.Web (DavRessource (..), DavTag (..))
import qualified Korrvigs.Utils.DAV.Web as Web
import Korrvigs.Utils.JSON (fromJSONM)
import Korrvigs.Utils.Time (measureTimeMs)
import Opaleye hiding (null)

setupCDD :: (MonadKorrvigs m) => AddressBook -> Text -> m DAV.CardDavData
setupCDD abook pwd = do
  man <- manager
  pure $
    DAV.CardDavData
      { DAV._cardUser = abook ^. abookUser,
        DAV._cardPwd = pwd,
        DAV._cardManager = man,
        DAV._cardServer = abook ^. abookServer,
        DAV._cardAbook = abook ^. abookName
      }

reportErr :: (Text -> m ()) -> Web.DavError -> m ()
reportErr report err =
  report $ "DAV error (" <> T.pack (show $ err ^. Web.davStatusCode) <> "): " <> err ^. Web.davError

pullAddressBook :: (MonadKorrvigs m) => (Text -> m ()) -> AddressBook -> Text -> m Bool
pullAddressBook report abook pwd =
  isJust <$> runMaybeT (pullAddressBookImpl report abook pwd)

pullAddressBookImpl :: (MonadKorrvigs m) => (Text -> m ()) -> AddressBook -> Text -> MaybeT m ()
pullAddressBookImpl report abook pwd = do
  let i = abook ^. abookEntry . entryName
  lift $ report $ "> Pulling from " <> unId i
  (time, mtdt) <- measureTimeMs $ do
    cdd <- lift $ setupCDD abook pwd
    nctag <-
      lift (DAV.getCTag cdd) >>= \case
        Left err -> reportE err >> mzero
        Right ctag -> pure ctag
    ctag <- lift $ rSelectTextMtdt DAVCTag $ sqlId i
    if ctag == Just (extractDavTag nctag)
      then do
        lift $ report $ "Nothing to do for " <> unId i
        pure M.empty
      else do
        pullAndMerge report abook cdd
        pure $ M.singleton (mtdtSqlName DAVCTag) (toJSON nctag)
  date <- liftIO getCurrentTime
  let metaMtdt =
        M.fromList
          [ (mtdtSqlName RunTime, toJSON time),
            (mtdtSqlName RunDate, toJSON $ iso8601Show date)
          ]
  lift $ updateMetadata (abook ^. abookEntry) (M.union metaMtdt mtdt) []
  pure undefined
  where
    reportE = lift . reportErr report

pullAndMerge :: (MonadKorrvigs m) => (Text -> m ()) -> AddressBook -> DAV.CardDavData -> MaybeT m ()
pullAndMerge report abook cdd = do
  lift $ report ">> Download ETags"
  netags <-
    DAV.getETags cdd >>= \case
      Left err -> lift (reportErr report err) >> mzero
      Right etags -> pure etags
  etags' <- lift $ rSelect $ do
    entry <- selectTable entriesTable
    let i = entry ^. sqlEntryId
    vcard <- baseSelectMtdt VCardMtdt i
    pure (entry ^. sqlEntryName, vcard)
  let vcdats :: Map Id [VCardData] = M.fromList $ mapMaybe (\(i, vcard) -> (i,) <$> fromJSONM vcard) etags'
  let etags = M.fromList $ concatMap (\(i, vdats) -> (\vdat -> (vdat ^. vcdatPath, (i, vdat ^. vcdatETag, vdat ^. vcdatUID))) <$> vdats) $ M.toList vcdats
  let uids = M.fromList $ concatMap (\(i, vdats) -> (\vdat -> (vdat ^. vcdatUID, i)) <$> vdats) $ M.toList vcdats

  -- Get data for all new and changed
  let newContacts = M.difference netags etags
  let changed = M.filter (\(_, netag, etag, _) -> netag /= etag) $ M.intersectionWith (\netag (i, etag, uid) -> (i, netag, etag, uid)) netags etags
  let toget = M.keys newContacts ++ M.keys changed
  lift $ report $ ">> Downloading data for " <> T.pack (show $ length toget) <> " contacts"
  ndata <-
    lift (DAV.getCardData cdd toget) >>= \case
      Left err -> lift (reportErr report err) >> mzero
      Right dat -> pure dat

  -- Remove contacts that where remotely deleted
  let del = M.difference etags netags
  lift $ report $ ">> Removing " <> T.pack (show $ M.size del) <> " remotely deleted contacts"
  forM_ (M.toList del) $ \(path, (i, _, _)) -> do
    lift $ report $ ">>> Removing card " <> extractDavRc path <> " from " <> unId i
    ev <-
      lift (load i) >>= \case
        Just ev -> pure ev
        Nothing -> lift (report $ "Failed to load event " <> unId i) >> mzero
    vdat <- hoistLift $ rSelectMtdt VCardMtdt ev
    let nvdat = filter (\vc -> vc ^. vcdatPath /= path) vdat
    lift $ updateMetadata ev (M.singleton (mtdtSqlName VCardMtdt) (toJSON nvdat)) []

  -- Download new contacts
  lift $ report $ ">> Creating " <> T.pack (show $ M.size newContacts) <> " new contacts"
  forM_ (M.toList newContacts) $ \(davref, etag) -> do
    lift $ report $ ">>> Creating for " <> extractDavRc davref
    dat <- hoistMaybe $ M.lookup davref ndata
    vcard <-
      liftIO (parseVCard Nothing $ LEnc.encodeUtf8 $ LT.fromStrict dat) >>= \case
        Left err -> lift (report $ "Failed to parse data for " <> extractDavRc davref <> ": " <> err) >> mzero
        Right vcard -> pure vcard
    case M.lookup (vcard ^. vcUID) uids of
      Just i -> do
        lift $ report $ "Merging with " <> unId i
        mergeInto report i vcard (vcard ^. vcUID) etag davref
      Nothing -> do
        i <- newFrom vcard (abook ^. abookEntry . entryName) etag davref
        lift $ report $ "Created new " <> unId i

  -- Updates contacts present on both
  lift $ report $ ">> Updating " <> T.pack (show $ M.size changed) <> " contacts"
  forM_ (M.toList changed) $ \(davref, (i, netag, _, uid)) -> do
    lift $ report $ ">>> Updating " <> unId i
    dat <- hoistMaybe $ M.lookup davref ndata
    vcard <-
      liftIO (parseVCard Nothing $ LEnc.encodeUtf8 $ LT.fromStrict dat) >>= \case
        Left err -> lift (report $ "Failed to parse data for " <> extractDavRc davref <> ": " <> err) >> mzero
        Right ical -> pure ical
    mergeInto report i vcard uid netag davref

newFrom :: (MonadKorrvigs m) => VCardFile -> Id -> DavTag -> DavRessource -> MaybeT m Id
newFrom vcard abook etag davref = do
  let nm = fromMaybe "New contact" $ vcard ^. vcFullName
  let vdat =
        VCardData
          { _vcdatUID = vcard ^. vcUID,
            _vcdatPath = davref,
            _vcdatETag = etag
          }
  let mtdt = M.fromList [(mtdtName VCardMtdt, toJSON [vdat])]
  let nnote =
        NewNote
          { _nnEntry = def & neMtdt .~ mtdt & neParents .~ [abook],
            _nnTitle = nm,
            _nnTitleOverride = False,
            _nnIgnoreUrl = True,
            _nnAllowDuplicateTitle = True
          }
  i <- lift $ new nnote
  entry <- hoistLift $ load i
  lift $ mergeFromVCard entry vcard
  pure i

mergeInto :: (MonadKorrvigs m) => (Text -> m ()) -> Id -> VCardFile -> Text -> DavTag -> DavRessource -> MaybeT m ()
mergeInto report i vcard uid netag davref = do
  entry <-
    lift (load i) >>= \case
      Nothing -> lift (report $ "Failed to load " <> unId i) >> mzero
      Just entry -> pure entry
  oldvcard <- hoistLift $ rSelectMtdt VCardMtdt entry
  let nvcard =
        oldvcard
          & each . filtered (\vc -> vc ^. vcdatUID == uid)
            %~ (vcdatETag .~ netag) . (vcdatPath .~ davref)
  let mtdt = M.singleton (mtdtSqlName VCardMtdt) (toJSON nvcard)
  lift $ updateMetadata entry mtdt []
  lift $ mergeFromVCard entry vcard
