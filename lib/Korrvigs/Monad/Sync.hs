module Korrvigs.Monad.Sync (sync, syncFile, syncFileOfKind, syncOne, remove) where

import Conduit (throwM)
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.CaseInsensitive (CI)
import Data.Foldable
import Data.List (singleton)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import qualified Database.PostgreSQL.Simple as Simple
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Entry
import qualified Korrvigs.Event.Sync as Event
import qualified Korrvigs.File.Sync as File
import Korrvigs.Kind
import Korrvigs.Monad.Class
import Korrvigs.Monad.SQL
import qualified Korrvigs.Note.Sync as Note
import qualified Korrvigs.Syndicate.Sync as Syn
import Korrvigs.Utils.Cycle
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Time (measureTime, measureTime_)
import Opaleye hiding (not, null)
import System.FilePath
import Prelude hiding (putStrLn)

remove :: (MonadKorrvigs m) => Entry -> m ()
remove entry = do
  removeDB entry
  case entry ^. entryKindData of
    NoteD note -> Note.remove note
    FileD file -> File.remove file
    EventD ev -> Event.remove ev
    CalendarD cal -> Cal.remove cal
    SyndicateD syn -> Syn.remove syn

loadIDsFor :: forall m. (MonadKorrvigs m) => Text -> (FilePath -> Id) -> m (Set FilePath) -> m (Map Id [FilePath])
loadIDsFor kdTxt extractId doList = do
  (tm, st) <- measureTime doList
  liftIO $ putStrLn $ kdTxt <> ": listed " <> T.pack (show $ S.size st) <> " in " <> tm
  pure . M.fromListWith (<>) . S.toList $ S.map (extractId &&& singleton) st

loadIDsOn :: (MonadKorrvigs m) => Kind -> m (Map Id [FilePath])
loadIDsOn Note = loadIDsFor (displayKind Note) Note.noteIdFromPath Note.list
loadIDsOn File = loadIDsFor (displayKind File) File.fileIdFromPath File.list
loadIDsOn Event = loadIDsFor (displayKind Event) (fst . Event.eventIdFromPath) Event.list
loadIDsOn Calendar = loadIDsFor (displayKind Calendar) Cal.calIdFromPath Cal.list
loadIDsOn Syndicate = loadIDsFor (displayKind Syndicate) Syn.synIdFromPath Syn.list

loadIDs :: (MonadKorrvigs m) => m (Map Id [FilePath])
loadIDs = do
  allIDs <- mapM loadIDsOn [minBound .. maxBound]
  pure $ M.unionsWith (<>) allIDs

sqlIDs :: (MonadKorrvigs m) => m (Map Id [Text])
sqlIDs = do
  ids <- rSelect $ do
    erow <- selectTable entriesTable
    pure $ erow ^. sqlEntryName
  pure $ M.fromList $ (MkId &&& const []) <$> ids

runSync ::
  (MonadKorrvigs m) =>
  Text ->
  m (Map Id SyncData) ->
  m (Map Id ([Id], [Id], m ()))
runSync kdTxt dt = do
  (tm, r) <- measureTime dt
  liftIO $ putStrLn $ kdTxt <> ": synced " <> T.pack (show $ M.size r) <> " in " <> tm
  let handleSyncData sdt = (sdt ^. syncParents, sdt ^. syncRefs, syncSQL sdt)
  pure $ handleSyncData . fixSyncData <$> r

runSyncOn :: (MonadKorrvigs m) => Kind -> m (Map Id ([Id], [Id], m ()))
runSyncOn Note = runSync (displayKind Note) Note.sync
runSyncOn File = runSync (displayKind File) File.sync
runSyncOn Event = runSync (displayKind Event) Event.sync
runSyncOn Calendar = runSync (displayKind Calendar) Cal.sync
runSyncOn Syndicate = runSync (displayKind Syndicate) Syn.sync

nameToIdMap :: (MonadKorrvigs m) => m (Map Id Int)
nameToIdMap = do
  nameIds <- rSelect $ (view sqlEntryName &&& view sqlEntryId) <$> selectTable entriesTable
  pure $ M.fromList nameIds

prepare :: Map Id Int -> (Id, Id) -> Maybe (Int, Int)
prepare mp (nm1, nm2) = (,) <$> M.lookup nm1 mp <*> M.lookup nm2 mp

sync :: (MonadKorrvigs m) => m ()
sync = do
  withSQL $ \conn ->
    void $ liftIO $ Simple.execute_ conn "truncate entries_sub, entries_ref_to, computations"
  ids <- loadIDs
  let conflict = M.toList $ M.filter ((>= 2) . length) ids
  unless (null conflict) $ throwM $ KDuplicateId conflict
  sqls <- sqlIDs
  let toRemove = view _1 <$> M.toList (M.difference sqls ids)
  rmT <- measureTime_ $ forM_ toRemove $ \i -> do
    entry <- load i
    forM_ entry remove
  liftIO $ putStrLn $ "Removed " <> T.pack (show $ length toRemove) <> " entries in " <> rmT
  allrels <- mapM runSyncOn [minBound .. maxBound]
  let rels = foldl' M.union M.empty allrels
  let checkRD = checkRelData $ \i -> isJust $ M.lookup i ids
  case foldr (firstJust . checkRD . (view _1 &&& view _2) . view _2) Nothing (M.toList rels) of
    Nothing -> pure ()
    Just i -> throwM $ KRelToUnknown i
  case hasCycle (view _1 <$> rels) of
    Nothing -> pure ()
    Just cle -> throwM $ KSubCycle cle
  let subBindings = mkBindings $ view _1 <$> rels
  let refBindings = mkBindings $ view _2 <$> rels
  dbT <- measureTime_ $ do
    forM_ rels $ view _3
    nameToId <- nameToIdMap
    let subs = mapMaybe (prepare nameToId) subBindings
    let refs = mapMaybe (prepare nameToId) refBindings
    atomicInsert [insertSubOf subs, insertRefTo refs]
  liftIO $ putStrLn $ "Updated database in " <> dbT
  where
    mkBindings :: Map a [a] -> [(a, a)]
    mkBindings m = (\(a, l) -> (a,) <$> l) =<< M.toList m
    checkRelData :: (Id -> Bool) -> ([Id], [Id]) -> Maybe Id
    checkRelData check (subOf, refTo) =
      firstJust
        (find (not . check) subOf)
        (find (not . check) refTo)
    firstJust :: Maybe a -> Maybe a -> Maybe a
    firstJust (Just x) _ = Just x
    firstJust Nothing x = x

identifyPath :: forall m. (MonadKorrvigs m) => FilePath -> m Kind
identifyPath path = do
  roots <- mapM (\kd -> (kd,) <$> kdRoot kd) [minBound .. maxBound]
  case find (isRel . snd) roots of
    Nothing -> throwM $ KMiscError $ "\"" <> T.pack path <> "\" is not a valid path entry"
    Just (kd, _) -> pure kd
  where
    isRel :: FilePath -> Bool
    isRel rt = isRelative $ makeRelative rt path
    kdRoot :: Kind -> m FilePath
    kdRoot Note = Note.noteDirectory
    kdRoot File = File.filesDirectory
    kdRoot Event = Event.eventsDirectory
    kdRoot Calendar = Cal.calendarsDirectory
    kdRoot Syndicate = Syn.syndicatesDirectory

syncFileImpl ::
  (MonadKorrvigs m) =>
  Id ->
  SyncData ->
  m ()
syncFileImpl i sdt' = do
  let sdt = fixSyncData sdt'
  syncSQL sdt
  nameToId <- nameToIdMap
  forM_ (M.lookup i nameToId) $ \sqlI ->
    syncRelsSQL
      sqlI
      (mapMaybe (`M.lookup` nameToId) $ sdt ^. syncParents)
      (mapMaybe (`M.lookup` nameToId) $ sdt ^. syncRefs)

fixSyncData :: SyncData -> SyncData
fixSyncData sdt = sdt & syncRefs %~ (addRefs ++)
  where
    addRefs = sdt ^. syncMtdtRows >>= uncurry refsFromMetadata

refsFromMetadata :: CI Text -> Value -> [Id]
refsFromMetadata mtdt val
  | mtdt `S.member` idMetadata =
      toList $ MkId <$> fromJSONM val
refsFromMetadata _ _ = []

syncFileOfKind :: (MonadKorrvigs m) => FilePath -> Kind -> m ()
syncFileOfKind path Note =
  syncFileImpl (Note.noteIdFromPath path) =<< Note.syncOne path
syncFileOfKind path File =
  syncFileImpl (File.fileIdFromPath path) =<< File.syncOne path
syncFileOfKind path Event =
  syncFileImpl (fst $ Event.eventIdFromPath path) =<< Event.syncOne path
syncFileOfKind path Calendar =
  syncFileImpl (Cal.calIdFromPath path) =<< Cal.syncOne path
syncFileOfKind path Syndicate =
  syncFileImpl (Syn.synIdFromPath path) =<< Syn.syncOne path

syncFile :: (MonadKorrvigs m) => FilePath -> m ()
syncFile path = identifyPath path >>= syncFileOfKind path

syncOne :: (MonadKorrvigs m) => Entry -> m ()
syncOne entry = case entry ^. entryKindData of
  NoteD note -> syncFileOfKind (note ^. notePath) Note
  FileD file -> syncFileOfKind (file ^. filePath) File
  EventD event -> syncFileOfKind (event ^. eventFile) Event
  CalendarD cal -> Cal.calendarPath cal >>= flip syncFileOfKind Calendar
  SyndicateD syn -> syncFileOfKind (syn ^. synPath) Syndicate
