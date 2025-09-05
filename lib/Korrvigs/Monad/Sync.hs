module Korrvigs.Monad.Sync (sync, syncFile, syncFileOfKind, syncOne) where

import Conduit (throwM)
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.List (find, foldl', singleton)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Profunctor.Product.Default
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import qualified Database.PostgreSQL.Simple as Simple
import qualified Korrvigs.Calendar.SQL as CalS
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Entry
import qualified Korrvigs.Event.SQL as EventS
import qualified Korrvigs.Event.Sync as Event
import qualified Korrvigs.File.SQL as FileS
import qualified Korrvigs.File.Sync as File
import Korrvigs.Kind
import qualified Korrvigs.Link.SQL as LinkS
import qualified Korrvigs.Link.Sync as Link
import Korrvigs.Monad.Class
import Korrvigs.Monad.Remove
import Korrvigs.Monad.SQL
import qualified Korrvigs.Note.SQL as NoteS
import qualified Korrvigs.Note.Sync as Note
import Korrvigs.Utils.Cycle
import Korrvigs.Utils.Time (measureTime, measureTime_)
import Opaleye hiding (not, null)
import System.FilePath
import Prelude hiding (putStrLn)

loadIDsFor :: forall m. (MonadKorrvigs m) => Text -> (FilePath -> Id) -> m (Set FilePath) -> m (Map Id [FilePath])
loadIDsFor kdTxt extractId doList = do
  (tm, st) <- measureTime doList
  liftIO $ putStrLn $ kdTxt <> ": listed " <> T.pack (show $ S.size st) <> " in " <> tm
  pure . M.fromListWith (<>) . S.toList $ S.map (extractId &&& singleton) st

loadIDsOn :: (MonadKorrvigs m) => Kind -> m (Map Id [FilePath])
loadIDsOn Link = loadIDsFor (displayKind Link) Link.linkIdFromPath Link.list
loadIDsOn Note = loadIDsFor (displayKind Note) Note.noteIdFromPath Note.list
loadIDsOn File = loadIDsFor (displayKind File) File.fileIdFromPath File.list
loadIDsOn Event = loadIDsFor (displayKind Event) (fst . Event.eventIdFromPath) Event.list
loadIDsOn Calendar = loadIDsFor (displayKind Calendar) Cal.calIdFromPath Cal.list

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
  (MonadKorrvigs m, Default ToFields r sql) =>
  Text ->
  Table sql sql ->
  m (Map Id (SyncData r)) ->
  m (Map Id ([Id], [Id], m ()))
runSync kdTxt tbl dt = do
  (tm, r) <- measureTime dt
  liftIO $ putStrLn $ kdTxt <> ": synced " <> T.pack (show $ M.size r) <> " in " <> tm
  pure $ (\sdt -> (sdt ^. syncParents, sdt ^. syncRefs, syncSQL tbl sdt)) <$> r

runSyncOn :: (MonadKorrvigs m) => Kind -> m (Map Id ([Id], [Id], m ()))
runSyncOn Link = runSync (displayKind Link) LinkS.linksTable Link.sync
runSyncOn Note = runSync (displayKind Note) NoteS.notesTable Note.sync
runSyncOn File = runSync (displayKind File) FileS.filesTable File.sync
runSyncOn Event = runSync (displayKind Event) EventS.eventsTable Event.sync
runSyncOn Calendar = runSync (displayKind Calendar) CalS.calendarsTable Cal.sync

nameToIdMap :: (MonadKorrvigs m) => m (Map Id Int)
nameToIdMap = do
  nameIds <- rSelect $ (view sqlEntryName &&& view sqlEntryId) <$> selectTable entriesTable
  pure $ M.fromList nameIds

prepare :: Map Id Int -> (Id, Id) -> Maybe (Int, Int)
prepare mp (nm1, nm2) = (,) <$> M.lookup nm1 mp <*> M.lookup nm2 mp

sync :: (MonadKorrvigs m) => m ()
sync = do
  conn <- pgSQL
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
  case hasCycle (view _2 <$> rels) of
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
    kdRoot Link = Link.linksDirectory
    kdRoot Note = Note.noteDirectory
    kdRoot File = File.filesDirectory
    kdRoot Event = Event.eventsDirectory
    kdRoot Calendar = Cal.calendarsDirectory

syncFileImpl ::
  (MonadKorrvigs m, Default ToFields hs sql) =>
  Id ->
  Table sql sql ->
  SyncData hs ->
  m ()
syncFileImpl i tbl sdt = do
  syncSQL tbl sdt
  nameToId <- nameToIdMap
  forM_ (M.lookup i nameToId) $ \sqlI ->
    syncRelsSQL
      sqlI
      (mapMaybe (`M.lookup` nameToId) $ sdt ^. syncParents)
      (mapMaybe (`M.lookup` nameToId) $ sdt ^. syncRefs)

syncFileOfKind :: (MonadKorrvigs m) => FilePath -> Kind -> m ()
syncFileOfKind path Link =
  syncFileImpl (Link.linkIdFromPath path) LinkS.linksTable =<< Link.syncOne path
syncFileOfKind path Note =
  syncFileImpl (Note.noteIdFromPath path) NoteS.notesTable =<< Note.syncOne path
syncFileOfKind path File =
  syncFileImpl (File.fileIdFromPath path) FileS.filesTable =<< File.syncOne path
syncFileOfKind path Event =
  syncFileImpl (fst $ Event.eventIdFromPath path) EventS.eventsTable =<< Event.syncOne path
syncFileOfKind path Calendar =
  syncFileImpl (Cal.calIdFromPath path) CalS.calendarsTable =<< Cal.syncOne path

syncFile :: (MonadKorrvigs m) => FilePath -> m ()
syncFile path = identifyPath path >>= syncFileOfKind path

syncOne :: (MonadKorrvigs m) => Entry -> m ()
syncOne entry = case entry ^. entryKindData of
  LinkD link -> syncFileOfKind (link ^. linkPath) Link
  NoteD note -> syncFileOfKind (note ^. notePath) Note
  FileD file -> syncFileOfKind (file ^. filePath) File
  EventD event -> syncFileOfKind (event ^. eventFile) Event
  CalendarD cal -> Cal.calendarPath cal >>= flip syncFileOfKind Calendar
