module Korrvigs.Calendar.DAV (sync, DAVCTag (..), DAVPath (..), DAVETag (..)) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Time.LocalTime
import Korrvigs.Calendar (listCalendars)
import Korrvigs.Entry
import Korrvigs.Event.ICalendar
import Korrvigs.Event.SQL
import Korrvigs.Event.Sync (eventsDirectory)
import qualified Korrvigs.Event.Sync as Ev
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.TH
import Korrvigs.Monad
import Korrvigs.Monad.Metadata
import Korrvigs.Monad.Remove
import Korrvigs.Monad.Sync (syncFileOfKind)
import qualified Korrvigs.Utils.DAV.Cal as DAV
import Korrvigs.Utils.DAV.Web (DavRessource (..), DavTag (..))
import qualified Korrvigs.Utils.DAV.Web as Web
import Korrvigs.Utils.DateTree
import Network.URI hiding (path)
import Opaleye hiding (null)

-- New
mkMtdt "DAVCTag" "ctag" [t|Text|]
mkMtdt "DAVPath" "davpath" [t|Text|]
mkMtdt "DAVETag" "etag" [t|Text|]

setupCDD :: (MonadKorrvigs m) => Calendar -> Text -> m DAV.CalDavData
setupCDD cal pwd = do
  man <- manager
  pure $
    DAV.CalDavData
      { DAV._calUser = cal ^. calUser,
        DAV._calPwd = pwd,
        DAV._calManager = man,
        DAV._calServer = cal ^. calServer,
        DAV._calCalendar = cal ^. calName
      }

reportErr :: (Text -> m ()) -> Web.DavError -> m ()
reportErr report err =
  report $ "DAV error (" <> T.pack (show $ err ^. Web.davStatusCode) <> "): " <> err ^. Web.davError

sync :: (MonadKorrvigs m) => (Text -> m ()) -> Text -> m Bool
sync report pwd = fmap isJust $ runMaybeT $ do
  cals <- lift listCalendars
  forM_ cals $ \cal -> do
    let i = cal ^. calEntry . entryName
    lift $ report $ "> Syncing " <> unId i
    cdd <- lift $ setupCDD cal pwd
    nctag <-
      lift (DAV.getCTag cdd) >>= \case
        Left err -> reportE err >> mzero
        Right ctag -> pure ctag
    ctag <- lift $ rSelectTextMtdt DAVCTag $ sqlId i
    if ctag == Just (extractDavTag nctag)
      then lift $ report $ "Nothing to do for " <> unId i
      else do
        pullAndMerge report cal cdd
        pushNew report cal cdd
        lift $ updateMetadata (cal ^. calEntry) (M.singleton (mtdtSqlName DAVCTag) (toJSON nctag)) []
  where
    reportE = lift . reportErr report

pullAndMerge :: (MonadKorrvigs m) => (Text -> m ()) -> Calendar -> DAV.CalDavData -> MaybeT m ()
pullAndMerge report cal cdd = do
  lift $ report ">> Download ETags"
  netags <-
    DAV.getETags cdd >>= \case
      Left err -> lift (reportErr report err) >> mzero
      Right etags -> pure etags
  etags' <- lift $ rSelect $ do
    ev <- selectTable eventsTable
    where_ $ ev ^. sqlEventCalendar .== sqlId (cal ^. calEntry . entryName)
    let i = ev ^. sqlEventId
    res <- baseSelectTextMtdt DAVPath i
    etag <- baseSelectTextMtdt DAVETag i
    nm <- nameFor i
    pure (res, (nm, etag))
  let etags = M.fromList $ (DavRc *** (MkId *** DavTag)) <$> etags'

  -- Get data for all new and changed
  let new = M.difference netags etags
  let changed = M.filter (\(_, netag, etag) -> netag /= etag) $ M.intersectionWith (\netag (i, etag) -> (i, netag, etag)) netags etags
  let toget = M.keys new ++ M.keys changed
  lift $ report $ ">> Downloading data for " <> T.pack (show $ length toget) <> " events"
  ndata <-
    lift (DAV.getCalData cdd toget) >>= \case
      Left err -> lift (reportErr report err) >> mzero
      Right dat -> pure dat

  -- Remove events that where remotely deleted
  let del = M.difference etags netags
  lift $ report $ ">> Removing " <> T.pack (show $ M.size del) <> " remotely deleted events"
  forM_ del $ \(i, _) -> do
    lift $ report $ ">>> Removing " <> unId i
    ev <-
      lift (load i) >>= \case
        Just ev -> pure ev
        Nothing -> lift (report $ "Failed to load event " <> unId i) >> mzero
    lift $ removeDWIM ev

  -- Download new events
  lift $ report $ ">> Creating " <> T.pack (show $ M.size new) <> " new events"
  forM_ (M.toList new) $ \(davref, etag) -> do
    lift $ report $ ">>> Creating for " <> extractDavRc davref
    dat <- hoistMaybe $ M.lookup davref ndata
    ical' <-
      liftIO (parseICal Nothing $ LEnc.encodeUtf8 $ LT.fromStrict dat) >>= \case
        Left err -> lift (report $ "Failed to parse data for " <> extractDavRc davref <> ": " <> err) >> mzero
        Right ical -> pure ical
    ievent' <- case ical' ^. icEvent of
      Nothing -> lift (report $ extractDavRc davref <> " is not an event") >> mzero
      Just ievent -> pure ievent
    mi <- lift $ rSelectOne $ do
      ev <- selectTable eventsTable
      where_ $ ev ^. sqlEventUID .== sqlStrictText (ievent' ^. iceUid)
      nameFor $ ev ^. sqlEventId
    case mi of
      Just i -> do
        lift $ report $ "Merging with " <> unId i
        mergeInto report i ical' etag davref
      Nothing -> do
        let ievent =
              ievent'
                & iceMtdt . at (mtdtName DAVPath) ?~ toJSON davref
                & iceMtdt . at (mtdtName DAVETag) ?~ toJSON etag
        let ical = ical' & icEvent ?~ ievent
        i <- lift $ Ev.register ical
        let basename = unId i <> "_" <> unId (cal ^. calEntry . entryName) <> ".ics"
        let start = resolveICalTime ical <$> ievent ^. iceStart
        let day = localDay . zonedTimeToLocalTime <$> start
        rt <- lift eventsDirectory
        stored <- storeFile rt Ev.eventTreeType day basename $ FileLazy $ renderICalFile ical
        lift $ syncFileOfKind stored Event

  -- Updates events present on both
  lift $ report $ ">> Updating " <> T.pack (show $ M.size changed) <> " events"
  forM_ (M.toList changed) $ \(davref, (i, netag, _)) -> do
    lift $ report $ ">>> Updating " <> unId i
    dat <- hoistMaybe $ M.lookup davref ndata
    nical <-
      liftIO (parseICal Nothing $ LEnc.encodeUtf8 $ LT.fromStrict dat) >>= \case
        Left err -> lift (report $ "Failed to parse data for " <> extractDavRc davref <> ": " <> err) >> mzero
        Right ical -> pure ical
    mergeInto report i nical netag davref

mergeInto :: (MonadKorrvigs m) => (Text -> m ()) -> Id -> ICalFile -> DavTag -> DavRessource -> MaybeT m ()
mergeInto report i nical netag davref = do
  entry <-
    lift (load i) >>= \case
      Nothing -> lift (report $ "Failed to load " <> unId i) >> mzero
      Just entry -> pure entry
  ev <- case entry ^. entryKindData of
    EventD ev -> pure ev
    _ -> lift (report $ unId i <> " is not an event") >> mzero
  let path = ev ^. eventFile
  ical <-
    liftIO (parseICalFile path) >>= \case
      Left err -> lift (report $ "Failed to parse " <> T.pack path <> ": " <> err) >> mzero
      Right ical -> pure ical
  let newIcal =
        mergeICal ical nical
          & icEvent . _Just . iceMtdt . at (mtdtName DAVPath) ?~ toJSON davref
          & icEvent . _Just . iceMtdt . at (mtdtName DAVETag) ?~ toJSON netag
  liftIO $ BSL.writeFile path $ renderICalFile newIcal
  lift $ syncFileOfKind path Event

mergeICal :: ICalFile -> ICalFile -> ICalFile
mergeICal ic nic =
  ICFile
    { _icVersion = nic ^. icVersion,
      _icContent = nic ^. icContent,
      _icTimezones = nic ^. icTimezones,
      _icEvent = case (ic ^. icEvent, nic ^. icEvent) of
        (Just ev1, Just ev2) -> Just $ mergeICEvent ev1 ev2
        (Nothing, Just ev) -> Just ev
        (Just ev, Nothing) -> Just ev
        (Nothing, Nothing) -> Nothing
    }

mergeICEvent :: ICalEvent -> ICalEvent -> ICalEvent
mergeICEvent ev nev =
  nev
    & iceLocation .~ (ev ^. iceLocation <|> nev ^. iceLocation)
    & iceParents .~ nubOrd (ev ^. iceParents ++ nev ^. iceParents)
    & iceGeometry .~ (ev ^. iceGeometry <|> nev ^. iceGeometry)
    & iceMtdt .~ M.union (ev ^. iceMtdt) (nev ^. iceMtdt)

pushNew :: (MonadKorrvigs m) => (Text -> m ()) -> Calendar -> DAV.CalDavData -> MaybeT m ()
pushNew report cal cdd = do
  newEvents <- lift $ rSelect $ do
    ev <- selectTable eventsTable
    where_ $ ev ^. sqlEventCalendar .== sqlId (cal ^. calEntry . entryName)
    let i = ev ^. sqlEventId
    davref <- selectMtdt DAVPath i
    where_ $ isNull davref
    nm <- nameFor i
    pure (nm, ev ^. sqlEventFile)
  lift $ report $ ">> Uploading " <> T.pack (show $ length newEvents) <> " new events"
  forM_ newEvents $ \(i, file) -> do
    lift $ report $ ">>> Uploading " <> unId i
    content <- liftIO $ BSL.readFile file
    uriPth <- case parseURI (T.unpack $ cal ^. calServer) of
      Nothing -> lift (report $ "Failed to parse server " <> cal ^. calServer) >> mzero
      Just uri -> pure $ T.pack $ uriPath uri
    let evRc = DavRc $ uriPth <> "/calendars/" <> cal ^. calUser <> "/" <> cal ^. calName <> "/korr_" <> T.replace ":" "_" (unId i) <> ".ics"
    r <- lift $ DAV.putCalData cdd evRc Nothing content
    case r of
      Left err -> lift (reportErr report err) >> mzero
      Right netag -> do
        ical' <-
          liftIO (parseICal (Just file) content) >>= \case
            Left err -> lift (report $ "Failed to parse " <> T.pack file <> ": " <> err) >> mzero
            Right ical -> pure ical
        let ical =
              ical'
                & icEvent . _Just . iceMtdt . at (mtdtName DAVPath) ?~ toJSON evRc
                & icEvent . _Just . iceMtdt . at (mtdtName DAVETag) ?~ toJSON netag
        liftIO $ BSL.writeFile file $ renderICalFile ical
        lift $ syncFileOfKind file Event
