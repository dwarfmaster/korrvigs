module Korrvigs.Calendar.DAV (pull, sync) where

import Conduit (throwM)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
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
import Korrvigs.Event.Sync (dSyncOneImpl, eventsDirectory)
import qualified Korrvigs.Event.Sync as Ev
import Korrvigs.Monad hiding (sync)
import Korrvigs.Utils (partitionM)
import qualified Korrvigs.Utils.DAV.Cal as DAV
import Korrvigs.Utils.DateTree
import qualified Korrvigs.Utils.Git.Status as St
import Korrvigs.Utils.JSON (writeJsonToFile)
import Korrvigs.Utils.Process
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Opaleye hiding (null)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

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

checkChanges :: (MonadKorrvigs m) => Calendar -> Text -> Maybe Text -> Map Text (Text, FilePath) -> m CalChanges
checkChanges cal pwd ctag etags = do
  cdd <- setupCDD cal pwd
  nctag <- DAV.getCTag cdd >>= throwEither (\err -> KMiscError $ "Failed to get ctag for calendar \"" <> cal ^. calName <> "\": " <> T.pack (show err))
  if ctag == Just nctag
    then pure $ CalChanges nctag M.empty M.empty M.empty
    else do
      netags <- DAV.getETags cdd >>= throwEither (\err -> KMiscError $ "Failed to get etags for calendar \"" <> cal ^. calName <> "\": " <> T.pack (show err))
      let onServer = M.difference netags etags
      let local = view _2 <$> M.difference etags netags
      let candidates = M.differenceWith (\oldtag (ntag, _) -> if ntag /= oldtag then Just ntag else Nothing) netags etags
      let update = M.intersectionWith (const id) candidates etags
      pure $ CalChanges nctag onServer update local

downloadAndWrite :: (MonadKorrvigs m) => Calendar -> Text -> FilePath -> Map Text (Maybe FilePath) -> m (Map Text FilePath)
downloadAndWrite cal pwd rt toinsert = do
  cdd <- setupCDD cal pwd
  let insertUids = M.keys toinsert
  dat <- DAV.getCalData cdd insertUids >>= throwEither (\err -> KMiscError $ "Failed to download content for calendar \"" <> cal ^. calName <> "\": " <> T.pack (show err))
  forM dat $ \ics -> do
    ical <- liftIO (parseICal Nothing $ LEnc.encodeUtf8 $ LT.fromStrict ics) >>= throwEither (\err -> KMiscError $ "Failed to parse received ics: " <> err)
    (i, nical, ievent, _) <- Ev.register ical
    let pth = join $ M.lookup (ievent ^. iceUid) toinsert
    case pth of
      Just icspath -> do
        liftIO $ BSL.writeFile (rt </> icspath) $ renderICalFile nical
        pure icspath
      Nothing -> do
        let basename = unId i <> "_" <> unId (cal ^. calEntry . name) <> ".ics"
        let start = resolveICalTime ical <$> ievent ^. iceStart
        let day = localDay . zonedTimeToLocalTime <$> start
        stored <- storeFile rt Ev.eventTreeType day basename $ renderICalFile nical
        pure $ makeRelative rt stored

doPull :: (MonadKorrvigs m) => Calendar -> Text -> FilePath -> CalChanges -> m (Map Text FilePath)
doPull cal pwd rt changes = do
  let onServer = M.fromList $ (,Nothing) <$> M.keys (changes ^. calOnServer)
  evRt <- Ev.eventsDirectory
  let prepPath = joinPath . (\f -> [rt, f]) . makeRelative evRt
  let diff = M.map (Just . prepPath . view _2) $ changes ^. calDiff
  let toinsert = M.union onServer diff
  inserted <- downloadAndWrite cal pwd rt toinsert
  forM_ (changes ^. calLocal) $ \evpath -> do
    let rerooted = prepPath evpath
    exists <- liftIO $ doesFileExist rerooted
    when exists $ liftIO $ removeFile rerooted
  pure inserted

data CachedData = CachedData
  { _cachedCtag :: Maybe Text,
    _cachedEtags :: Map Text (Text, FilePath)
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
  insertedPaths <- doPull cal pwd worktreeRoot changes
  -- Cache tags
  let cmp = Computation i "dav" (Builtin Blt.CalDav) Json
  let ncomps = M.insert "dav" cmp comps
  storeComputations' ncomps file
  compPath <- compFile cmp >>= reroot
  let etags = changes ^. calOnServer <> fmap (view _1) (changes ^. calDiff)
  let etagsWithPath = M.intersectionWith (,) etags insertedPaths
  let ncached = CachedData (Just $ changes ^. calCTag) etagsWithPath
  writeJsonToFile compPath ncached

syncMsg :: [Text] -> Text
syncMsg cals = "Pulled calendars " <> T.intercalate ", " cals

sync :: (MonadKorrvigs m) => Bool -> [Calendar] -> Text -> m ()
sync restore cals pwd = do
  -- Pull from caldav
  forM_ cals $ \cal -> pull cal pwd

  -- We save the starting commit
  rt <- root
  mainCiRaw <- liftIO $ readCreateProcess ((proc "git" ["rev-parse", "main"]) {cwd = Just rt}) ""
  let mainCi = reverse $ dropWhile isSpace $ reverse mainCiRaw
  gitRoot <- liftIO $ readCreateProcess ((proc "git" ["rev-parse", "--show-toplevel"]) {cwd = Just rt}) ""

  -- Commit and merge
  wkrt <- root >>= reroot
  gstatus <- liftIO (St.gitStatus wkrt) >>= throwEither (\err -> KMiscError $ "git status failed: " <> err)
  unless (null gstatus) $ do
    void $ runSilentK (proc "git" ["add", joinPath [wkrt, "events"]]) {cwd = Just wkrt}
    void $ runSilentK (proc "git" ["add", joinPath [wkrt, "cache"]]) {cwd = Just wkrt}
    let msg = syncMsg $ view (calEntry . name . to unId) <$> cals
    void $ runSilentK (proc "git" ["commit", "-m", T.unpack msg]) {cwd = Just wkrt}

    -- We merge on main if there were changes
    gmerge <- runSilentK (proc "git" ["merge", "calsync"]) {cwd = Just rt}
    unless (gmerge == ExitSuccess) $ do
      when restore $ void $ runSilentK (proc "git" ["merge", "--abort"]) {cwd = Just rt}
      throwM $ KMiscError "Failed to merge calsync"

  -- We sync only the changed or added files, and remove from the database the
  -- removed files
  changedFilesRaw <- liftIO $ readCreateProcess ((proc "git" ["diff", "--name-only", "main", mainCi]) {cwd = Just rt}) ""
  let changedFiles = (\f -> joinPath [gitRoot, f]) <$> lines changedFilesRaw
  evDir <- eventsDirectory
  let eventsFiles = filter (isRelative . makeRelative evDir) changedFiles
  (addedFiles, removedFiles) <-
    partitionM (liftIO . doesFileExist) eventsFiles
  forM_ removedFiles $ \rmPath -> do
    rmId <- rSelectOne $ do
      erow <- selectTable eventsTable
      where_ $ erow ^. sqlEventFile .== sqlString rmPath
      pure $ erow ^. sqlEventName
    forM_ rmId remove
  forM_ addedFiles dSyncOneImpl

  -- We push the events and reset the calsync branch to main
  -- TODO we push the events
  void $ runSilentK (proc "git" ["reset", "--hard", "main"]) {cwd = Just wkrt}
