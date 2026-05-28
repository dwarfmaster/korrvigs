module Korrvigs.Monad.Sync (sync, syncFileOfKind, syncOne, remove) where

import Conduit (throwM)
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.CaseInsensitive (CI)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
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
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye (detectCycles, sqlUnnest)
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

loadIDsFor :: forall m. (MonadKorrvigs m) => Text -> (FilePath -> Id) -> m (Set FilePath) -> m (Map Id (NonEmpty FilePath))
loadIDsFor kdTxt extractId doList = do
  (tm, st) <- measureTime doList
  liftIO $ putStrLn $ kdTxt <> ": listed " <> T.pack (show $ S.size st) <> " in " <> tm
  pure . M.fromListWith (<>) . S.toList $ S.map (extractId &&& NE.singleton) st

loadIDsOn :: (MonadKorrvigs m) => Kind -> m (Map Id (NonEmpty FilePath))
loadIDsOn Note = loadIDsFor (displayKind Note) Note.noteIdFromPath Note.list
loadIDsOn File = loadIDsFor (displayKind File) File.fileIdFromPath File.list
loadIDsOn Event = loadIDsFor (displayKind Event) (fst . Event.eventIdFromPath) Event.list
loadIDsOn Calendar = loadIDsFor (displayKind Calendar) Cal.calIdFromPath Cal.list
loadIDsOn Syndicate = loadIDsFor (displayKind Syndicate) Syn.synIdFromPath Syn.list

loadIDs :: (MonadKorrvigs m) => m (Map Id (NonEmpty (Kind, FilePath)))
loadIDs = do
  allIDs <- mapM (\kd -> fmap (fmap (kd,)) <$> loadIDsOn kd) [minBound .. maxBound]
  pure $ M.unionsWith (<>) allIDs

nameToIdMap :: (MonadKorrvigs m) => m (Map Id Int)
nameToIdMap = do
  nameIds <- rSelect $ (view sqlEntryName &&& view sqlEntryId) <$> selectTable entriesTable
  pure $ M.fromList nameIds

runSync :: (MonadKorrvigs m) => Kind -> Id -> FilePath -> Int -> m SyncData
runSync Note = Note.syncOne
runSync File = File.syncOne
runSync Event = Event.syncOne
runSync Calendar = Cal.syncOne
runSync Syndicate = Syn.syncOne

doSync :: (MonadKorrvigs m) => (Id -> Maybe Int) -> Kind -> Id -> FilePath -> Int -> m ()
doSync lookupSqlI kd i path sqlI =
  syncSQL False lookupSqlI kd sqlI . fixSyncData =<< runSync kd i path sqlI

sync :: (MonadKorrvigs m) => m ()
sync = do
  withSQL $ \conn ->
    void $ liftIO $ Simple.execute_ conn "truncate entries, entries_metadata, entries_sub, entries_ref_to, computations, computations_dep, notes, notes_collections, files, events, calendars, syndicates, syndicated_items"
  ids <- loadIDs
  let conflict = (_2 %~ NE.toList . fmap snd) <$> M.toList (M.filter ((>= 2) . length) ids)
  unless (null conflict) $ throwM $ KDuplicateId conflict
  (recrT, entries) <- measureTime $ withSQL $ \conn -> liftIO $ do
    let rows :: [EntryRowW] = fmap (\(i, k) -> EntryRow Nothing k i Nothing Nothing Nothing Nothing Nothing) $ M.toList $ fmap (fst . NE.head) ids
    void $
      runInsert conn $
        Insert
          { iTable = entriesTable,
            iRows = toFields <$> rows,
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    sqls <- runSelect conn $ do
      entry <- selectTable entriesTable
      pure (entry ^. sqlEntryName, entry ^. sqlEntryId)
    let sqlsMap = M.fromList (sqls :: [(Id, Int)])
    pure $ M.intersectionWith (\i paths -> (i, fst (NE.head paths), snd (NE.head paths))) sqlsMap ids
  liftIO $ putStrLn $ "Recreated " <> T.pack (show $ M.size ids) <> " entries in " <> recrT
  let lookupId i = view _1 <$> M.lookup i entries
  fillT <- measureTime_ $
    forM_ (M.toList entries) $
      \(i, (sqlI, kd, path)) -> doSync lookupId kd i path sqlI
  liftIO $ putStrLn $ "Filled " <> T.pack (show $ M.size ids) <> " entries data in " <> fillT
  (cycT, cycles :: [Id]) <- measureTime $ rSelect $ do
    (_, cyc) <- limit 1 $ detectCycles (selectTable entriesSubTable) (view source) (view target)
    e <- sqlUnnest cyc
    nameFor e
  liftIO $ putStrLn $ "Checked for cycles in " <> cycT
  unless (null cycles) $ throwM $ KSubCycle cycles

syncFileImpl ::
  (MonadKorrvigs m) =>
  Kind ->
  Int ->
  SyncData ->
  m ()
syncFileImpl kd sqlI sdt' = do
  let sdt = fixSyncData sdt'
  nameToId <- nameToIdMap
  syncSQL True (flip M.lookup nameToId) kd sqlI sdt

fixSyncData :: SyncData -> SyncData
fixSyncData sdt = sdt & syncRefs %~ (addRefs ++)
  where
    addRefs = sdt ^. syncMtdtRows >>= uncurry refsFromMetadata

refsFromMetadata :: CI Text -> Value -> [Id]
refsFromMetadata mtdt val
  | mtdt `S.member` idMetadata =
      toList $ MkId <$> fromJSONM val
refsFromMetadata _ _ = []

syncFileOfKind :: (MonadKorrvigs m) => Id -> FilePath -> Int -> Kind -> m ()
syncFileOfKind i path sqlI kd =
  syncFileImpl kd sqlI =<< runSync kd i path sqlI

syncOne :: (MonadKorrvigs m) => Entry -> m ()
syncOne entry = case entry ^. entryKindData of
  NoteD note -> syncFileOfKind (entry ^. entryName) (note ^. notePath) (entry ^. entryId) Note
  FileD file -> syncFileOfKind (entry ^. entryName) (file ^. filePath) (entry ^. entryId) File
  EventD event -> syncFileOfKind (entry ^. entryName) (event ^. eventFile) (entry ^. entryId) Event
  CalendarD cal -> Cal.calendarPath cal >>= \path -> syncFileOfKind (entry ^. entryName) path (entry ^. entryId) Calendar
  SyndicateD syn -> syncFileOfKind (entry ^. entryName) (syn ^. synPath) (entry ^. entryId) Syndicate
