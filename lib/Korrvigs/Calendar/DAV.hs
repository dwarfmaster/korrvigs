module Korrvigs.Calendar.DAV where

import Conduit (throwM)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Time.LocalTime
import Korrvigs.Compute
import qualified Korrvigs.Compute.Builtin as Blt
import Korrvigs.Entry
import Korrvigs.Event.ICalendar
import Korrvigs.Event.SQL
import Korrvigs.Event.Sync (eventsDirectory)
import qualified Korrvigs.Event.Sync as Ev
import Korrvigs.Monad
import qualified Korrvigs.Utils.DAV.Cal as DAV
import Korrvigs.Utils.DateTree
import Korrvigs.Utils.JSON (writeJsonToFile)
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Opaleye
import System.Directory
import System.FilePath

data CalChanges = CalChanges
  { _calCTag :: Text,
    _calOnServer :: Map Text Text, -- ics only on server
    _calDiff :: Map Text (Text, FilePath), -- ics on both that have changed
    _calLocal :: Map Text FilePath -- ics only present locally
  }
  deriving (Eq, Show)

makeLenses ''CalChanges

setupCDD :: (MonadIO m) => Calendar -> Text -> m DAV.CalDavData
setupCDD cal pwd = do
  man <- liftIO $ newManager tlsManagerSettings
  pure $
    DAV.CalDavData
      { DAV._calUser = cal ^. calUser,
        DAV._calPwd = pwd,
        DAV._calManager = man,
        DAV._calServer = cal ^. calServer,
        DAV._calCalendar = cal ^. calName
      }

checkChanges :: (MonadKorrvigs m) => Calendar -> Text -> Maybe Text -> Map Text Text -> m CalChanges
checkChanges cal pwd ctag etags = do
  cdd <- setupCDD cal pwd
  nctag <- DAV.getCTag cdd >>= throwEither (\err -> KMiscError $ "Failed to get ctag for calendar \"" <> cal ^. calName <> "\": " <> T.pack (show err))
  if ctag == Just nctag
    then pure $ CalChanges nctag M.empty M.empty M.empty
    else do
      netags <- DAV.getETags cdd >>= throwEither (\err -> KMiscError $ "Failed to get etags for calendar \"" <> cal ^. calName <> "\": " <> T.pack (show err))
      localUids :: Map Text FilePath <- fmap M.fromList $ rSelect $ do
        ev <- selectTable eventsTable
        where_ $ ev ^. sqlEventCalendar .== sqlId (cal ^. calEntry . name)
        pure (ev ^. sqlEventUID, ev ^. sqlEventFile)
      let onServer = M.differenceWith (const $ const Nothing) netags localUids
      let local = M.differenceWith (const $ const Nothing) localUids netags
      let candidates = M.differenceWith (\oldtag ntag -> if ntag /= oldtag then Just ntag else Nothing) netags etags
      let update = M.intersectionWith (,) candidates localUids
      pure $ CalChanges nctag onServer update local

downloadAndWrite :: (MonadKorrvigs m) => Calendar -> Text -> FilePath -> Map Text (Maybe FilePath) -> m [FilePath]
downloadAndWrite cal pwd rt toinsert = do
  cdd <- setupCDD cal pwd
  let insertUids = M.keys toinsert
  dat <- DAV.getCalData cdd insertUids >>= throwEither (\err -> KMiscError $ "Failed to download content for calendar \"" <> cal ^. calName <> "\": " <> T.pack (show err))
  forM (M.elems dat) $ \ics -> do
    ical <- liftIO (parseICal Nothing $ LEnc.encodeUtf8 $ LT.fromStrict ics) >>= throwEither (\err -> KMiscError $ "Failed to parse received ics: " <> err)
    (i, nical, ievent, _) <- Ev.register ical
    let pth = join $ M.lookup (ievent ^. iceUid) toinsert
    case pth of
      Just icspath -> do
        liftIO $ BSL.writeFile icspath $ renderICalFile nical
        pure icspath
      Nothing -> do
        let basename = unId i <> "_" <> unId (cal ^. calEntry . name) <> ".ics"
        let start = resolveICalTime ical <$> ievent ^. iceStart
        let day = localDay . zonedTimeToLocalTime <$> start
        storeFile rt Ev.eventTreeType day basename $ renderICalFile nical

doPull :: (MonadKorrvigs m) => Calendar -> Text -> FilePath -> CalChanges -> m ()
doPull cal pwd rt changes = do
  let onServer = M.fromList $ (,Nothing) <$> M.keys (changes ^. calOnServer)
  evRt <- Ev.eventsDirectory
  let prepPath = joinPath . (\f -> [rt, f]) . makeRelative evRt
  let diff = M.map (Just . prepPath . view _2) $ changes ^. calDiff
  let toinsert = M.union onServer diff
  void $ downloadAndWrite cal pwd rt toinsert
  forM_ (changes ^. calLocal) $ \evpath -> do
    let rerooted = prepPath evpath
    exists <- liftIO $ doesFileExist rerooted
    when exists $ liftIO $ removeFile rerooted

data CachedData = CachedData
  { _cachedCtag :: Maybe Text,
    _cachedEtags :: Map Text Text
  }

makeLenses ''CachedData

instance ToJSON CachedData where
  toJSON (CachedData ctag etags) =
    object $ maybe [] (\c -> ["ctag" .= String c]) ctag ++ ["etags" .= etags]

instance FromJSON CachedData where
  parseJSON = withObject "CachedData" $ \obj ->
    CachedData <$> obj .:? "ctag" <*> obj .: "etags"

reroot :: (MonadKorrvigs m) => FilePath -> m FilePath
reroot pth = do
  rt <- root
  let rel = makeRelative rt pth
  pure $ joinPath [rt, "../../korrvigs-temp/calsync", rel]

pull :: (MonadKorrvigs m) => Calendar -> Text -> m ()
pull cal pwd = do
  let i = cal ^. calEntry . name
  -- Extract cached tags
  file <- compsFile i >>= reroot
  comps <- entryStoredComputations' i file
  cdata <- case M.lookup "dav" comps of
    Nothing -> pure $ CachedData Nothing M.empty
    Just cached | cached ^. cmpAction /= Builtin Blt.CalDav -> throwM $ KMiscError "dav computation of calendar is already used"
    Just cached ->
      compFile cached >>= reroot >>= getJsonComp' >>= \case
        Nothing -> throwM $ KMiscError "Failed to load cached tags for calendar DAV"
        Just js -> pure js
  -- Pull from CalDAV
  changes <- checkChanges cal pwd (cdata ^. cachedCtag) (cdata ^. cachedEtags)
  events <- eventsDirectory
  worktreeRoot <- reroot events
  doPull cal pwd worktreeRoot changes
  -- Cache tags
  let cmp = Computation i "dav" (Builtin Blt.CalDav) Json
  let ncomps = M.insert "dav" cmp comps
  storeComputations' ncomps file
  compPath <- compFile cmp >>= reroot
  let etags = changes ^. calOnServer <> fmap (view _1) (changes ^. calDiff)
  let ncached = CachedData (Just $ changes ^. calCTag) etags
  writeJsonToFile compPath ncached
