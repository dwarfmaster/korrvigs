module Korrvigs.Calendar.DAV (pull, push, sync, CachedData (..)) where

import Conduit (throwM)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isSpace)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Time.LocalTime
import Korrvigs.Actions (load, remove, syncFileOfKind)
import Korrvigs.Calendar.Sync (calendarPath, updateCache)
import Korrvigs.Compute
import Korrvigs.Compute.Action
import Korrvigs.Compute.Declare
import Korrvigs.Entry
import Korrvigs.Event.ICalendar
import Korrvigs.Event.SQL
import Korrvigs.Event.Sync (eventIdFromPath, eventsDirectory)
import qualified Korrvigs.Event.Sync as Ev
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Utils (partitionM)
import qualified Korrvigs.Utils.DAV.Cal as DAV
import Korrvigs.Utils.DAV.Web (DavRessource (..), DavTag (..))
import Korrvigs.Utils.DateTree
import qualified Korrvigs.Utils.Git.Status as St
import Korrvigs.Utils.Process
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Opaleye hiding (null)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

data CalChanges = CalChanges
  { _calCTag :: DavTag,
    _calOnServer :: Map DavRessource DavTag, -- ics only on server
    _calDiff :: Map DavRessource (DavTag, FilePath), -- ics on both that have changed
    _calSame :: Map DavRessource (DavTag, FilePath), -- ics on both that are the same
    _calLocal :: Map DavRessource FilePath -- ics only present locally
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

checkChanges :: (MonadKorrvigs m) => Calendar -> Text -> Maybe DavTag -> Map DavRessource (DavTag, FilePath) -> m CalChanges
checkChanges cal pwd ctag etags = do
  cdd <- setupCDD cal pwd
  nctag <- DAV.getCTag cdd >>= throwEither (\err -> KMiscError $ "Failed to get ctag for calendar \"" <> cal ^. calName <> "\": " <> T.pack (show err))
  if ctag == Just nctag
    then pure $ CalChanges nctag M.empty M.empty etags M.empty
    else do
      netags <- DAV.getETags cdd >>= throwEither (\err -> KMiscError $ "Failed to get etags for calendar \"" <> cal ^. calName <> "\": " <> T.pack (show err))
      let onServer = M.difference netags etags
      let local = view _2 <$> M.difference etags netags
      let candidates = M.differenceWith (\ntag (oldtag, _) -> if ntag /= oldtag then Just ntag else Nothing) netags etags
      let update = M.intersectionWith (\netg (_, pth) -> (netg, pth)) candidates etags
      let same = M.map snd $ M.filter (\(netg, (etg, _)) -> netg == etg) $ M.intersectionWith (,) netags etags
      pure $ CalChanges nctag onServer update same local

downloadAndWrite ::
  (MonadKorrvigs m) =>
  Calendar ->
  Text ->
  FilePath ->
  Map DavRessource (Maybe FilePath) ->
  Set Id ->
  m (Map DavRessource FilePath, Set Id)
downloadAndWrite cal pwd rt toinsert forbidden = do
  cdd <- setupCDD cal pwd
  let insertUids = M.keys toinsert
  dat <- DAV.getCalData cdd insertUids >>= throwEither (\err -> KMiscError $ "Failed to download content for calendar \"" <> cal ^. calName <> "\": " <> T.pack (show err))
  flip runStateT forbidden $ fmap (M.fromList . catMaybes) $ forM (M.toList dat) $ \(davR, ics) -> do
    ical <- lift $ liftIO (parseICal Nothing $ LEnc.encodeUtf8 $ LT.fromStrict ics) >>= throwEither (\err -> KMiscError $ "Failed to parse received ics: " <> err)
    ievent <- lift $ throwMaybe (KMiscError "Received ics has no VEVENT") $ ical ^. icEvent
    let pth = M.lookup davR toinsert
    case pth of
      Just (Just icspath) -> do
        liftIO $ BSL.writeFile (rt </> icspath) $ renderICalFile ical
        let (i, _) = Ev.eventIdFromPath icspath
        modify $ S.insert i
        pure $ Just (davR, icspath)
      Just Nothing -> do
        forbid <- get
        i <- lift $ Ev.register ical forbid
        put $ S.insert i forbid
        let basename = unId i <> "_" <> unId (cal ^. calEntry . name) <> ".ics"
        let start = resolveICalTime ical <$> ievent ^. iceStart
        let day = localDay . zonedTimeToLocalTime <$> start
        stored <- lift $ storeFile rt Ev.eventTreeType day basename $ FileLazy $ renderICalFile ical
        pure $ Just (davR, makeRelative rt stored)
      Nothing ->
        throwM $ KMiscError $ "Received an event that was not asked for: \"" <> ievent ^. iceUid <> "\""

doPull :: (MonadKorrvigs m) => Calendar -> Text -> FilePath -> CalChanges -> Set Id -> m (Map DavRessource FilePath, Set Id)
doPull cal pwd rt changes forbidden = do
  let onServer = M.fromList $ (,Nothing) <$> M.keys (changes ^. calOnServer)
  evRt <- Ev.eventsDirectory
  let diff = M.map (Just . makeRelative evRt . view _2) $ changes ^. calDiff
  let toinsert = M.union onServer diff
  inserted <- downloadAndWrite cal pwd rt toinsert forbidden
  let prepPath = joinPath . (\f -> [rt, f]) . makeRelative evRt
  forM_ (changes ^. calLocal) $ \evpath -> do
    let rerooted = prepPath evpath
    exists <- liftIO $ doesFileExist rerooted
    when exists $ liftIO $ removeFile rerooted
  pure inserted

data CachedData = CachedData
  { _cachedCtag :: Maybe DavTag,
    _cachedEtags :: Map DavRessource (DavTag, FilePath)
  }

makeLenses ''CachedData

instance ToJSON CachedData where
  toJSON (CachedData ctag etags) =
    object $ maybe [] (singleton . ("ctag" .=)) ctag ++ ["etags" .= etags]

instance FromJSON CachedData where
  parseJSON = withObject "CachedData" $ \obj ->
    CachedData <$> obj .:? "ctag" <*> obj .: "etags"

reroot :: (MonadKorrvigs m) => FilePath -> m FilePath
reroot pth = do
  rt <- root
  let rel = makeRelative rt pth
  calsyncRt <- calsyncRoot
  pure $ joinPath [calsyncRt, rel]

calsyncCache :: (MonadKorrvigs m) => m FilePath
calsyncCache = cacheDir >>= reroot

pull :: (MonadKorrvigs m) => Calendar -> Text -> Set Id -> m (Set Id)
pull cal pwd forbidden = do
  let i = cal ^. calEntry . name
  -- Extract cached tags
  let act = Cached Json $ cal ^. calCache
  calsyncRt <- calsyncCache
  cdata <- runJSON' calsyncRt act >>= throwMaybe (KMiscError $ "Failed to load cached data for calendar " <> unId i)
  -- Pull from CalDAV
  changes <- checkChanges cal pwd (cdata ^. cachedCtag) (cdata ^. cachedEtags)
  events <- eventsDirectory
  worktreeRoot <- reroot events
  (insertedPaths, nforbidden) <- doPull cal pwd worktreeRoot changes forbidden
  -- Cache tags
  let etags = changes ^. calOnServer <> fmap (view _1) (changes ^. calDiff)
  let etagsWithPath = M.union (changes ^. calSame) $ M.intersectionWith (,) etags insertedPaths
  let ncached = CachedData (Just $ changes ^. calCTag) etagsWithPath
  (hash, _) <- storeCachedJson' calsyncRt ncached
  calfile <- calendarPath cal >>= reroot
  updateCache i calfile hash
  pure nforbidden

syncMsg :: [Text] -> Text
syncMsg cals = "Pulled calendars " <> T.intercalate ", " cals

pushMsg :: [Text] -> Text
pushMsg cals = "Pushed calendars " <> T.intercalate ", " cals

push :: (MonadKorrvigs m) => Calendar -> Text -> [FilePath] -> [FilePath] -> m FilePath
push cal pwd add rm = do
  let i = cal ^. calEntry . name
  -- Extract cached etags
  let act = Cached Json $ cal ^. calCache
  calsyncRt <- calsyncCache
  cdata <- runJSON' calsyncRt act >>= throwMaybe (KMiscError $ "Failed to load cached data for calendar " <> unId i)
  let pathToRC = M.fromList $ (\(rc, (etg, pth)) -> (pth, (etg, rc))) <$> M.toList (cdata ^. cachedEtags)
  evDir <- eventsDirectory
  -- Push each file one by one
  cdd <- setupCDD cal pwd
  r <- forM add $ \evPath -> do
    let relPath = makeRelative evDir evPath
    let evI = fst $ eventIdFromPath evPath
    (evRC, etag) <- case M.lookup relPath pathToRC of
      Just (etag, evRC) -> pure (evRC, Just etag)
      Nothing -> pure (DavRc $ "/korr_" <> T.replace ":" "_" (unId evI) <> ".ics", Nothing)
    r <- DAV.putCalData cdd evRC etag =<< liftIO (BSL.readFile evPath)
    case r of
      Left err -> throwM $ KMiscError $ "Failed to upload event \"" <> T.pack evPath <> "\": " <> T.pack (show err)
      Right netag -> pure (evRC, netag, relPath)
  -- Remove
  forM_ rm $ \evPath -> case M.lookup evPath pathToRC of
    Nothing -> pure ()
    Just (etg, rc) ->
      DAV.deleteCalData cdd rc etg >>= \case
        Left err -> throwM $ KMiscError $ "Failed to delete event \"" <> extractDavRc rc <> "\" from calendar \"" <> unId i <> "\": " <> T.pack (show err)
        Right () -> pure ()
  -- Store new etags
  let ncData = foldr (\(rc, etg, pth) -> cachedEtags . at rc ?~ (etg, pth)) cdata r
  (hash, compPath) <- storeCachedJson' calsyncRt ncData
  calfile <- calendarPath cal >>= reroot
  updateCache i calfile hash
  pure compPath

sync :: (MonadKorrvigs m) => Bool -> [Calendar] -> Text -> m ()
sync restore cals pwd = do
  -- Pull from caldav
  foldM_ (\forbidden cal -> pull cal pwd forbidden) S.empty cals

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
    gmerge <- runSilentK (proc "git" ["merge", "calsync", "-sresolve"]) {cwd = Just rt}
    unless (gmerge == ExitSuccess) $ do
      when restore $ void $ runSilentK (proc "git" ["merge", "--abort"]) {cwd = Just rt}
      throwM $ KMiscError "Failed to merge calsync"

  -- We sync only the changed or added files, and remove from the database the
  -- removed files
  evDir <- eventsDirectory
  do
    changedFilesRaw <- liftIO $ readCreateProcess ((proc "git" ["diff", "--name-only", "main", mainCi]) {cwd = Just rt}) ""
    let changedFiles = (gitRoot </>) <$> lines changedFilesRaw
    let eventsFiles = filter (isRelative . makeRelative evDir) changedFiles
    (addedFiles, removedFiles) <-
      partitionM (liftIO . doesFileExist) eventsFiles
    forM_ removedFiles $ \rmPath -> do
      rmId <- rSelectOne $ do
        erow <- selectTable eventsTable
        where_ $ erow ^. sqlEventFile .== sqlString rmPath
        pure $ erow ^. sqlEventName
      rmEv <- join <$> forM rmId load
      forM_ rmEv remove
    forM_ addedFiles $ flip syncFileOfKind Event

  -- We push the events and reset the calsync branch to main
  comps <- do
    toPushRaw <- liftIO $ readCreateProcess ((proc "git" ["diff", "--name-only", "main", "calsync", "--", evDir]) {cwd = Just rt}) ""
    let toPush = (gitRoot </>) <$> lines toPushRaw
    (addedFiles, removedFiles) <-
      partitionM (liftIO . doesFileExist) toPush
    forM cals $ \cal -> do
      let isCalEv pth = Ev.eventIdFromPath pth ^. _2 == cal ^. calEntry . name
      let calFilesAdd = filter isCalEv addedFiles
      let calFilesRm = filter isCalEv removedFiles
      push cal pwd calFilesAdd calFilesRm

  -- We commit the changed computation data, if any
  void $ runSilentK (proc "git" $ "add" : comps) {cwd = Just rt}
  hasStaged <- runSilentK (proc "git" ["diff", "--cached", "--quiet"]) {cwd = Just rt}
  unless (hasStaged == ExitSuccess) $ do
    let msg = pushMsg $ view (calEntry . name . to unId) <$> cals
    void $ runSilentK (proc "git" ["commit", "-m", T.unpack msg]) {cwd = Just rt}

  -- We reset the calsync branch to the main branch
  void $ runSilentK (proc "git" ["reset", "--hard", "main"]) {cwd = Just wkrt}
