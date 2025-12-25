module Korrvigs.Monad.Metadata
  ( updateMetadata,
    updateParents,
    updateDate,
    updateTitle,
    listCompute,
    updateRefs,
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time.LocalTime
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Compute.SQL
import Korrvigs.Entry
import qualified Korrvigs.Event.Sync as Event
import qualified Korrvigs.File.Sync as File
import qualified Korrvigs.Kind as Kd
import Korrvigs.Monad.Class
import Korrvigs.Monad.SQL (idMetadata, indexedMetadata, loadSql)
import Korrvigs.Monad.Sync (syncOne)
import Korrvigs.Note.SQL
import qualified Korrvigs.Note.Sync as Note
import qualified Korrvigs.Syndicate.Sync as Syn
import Opaleye hiding (not, null)
import qualified Opaleye as O

-- Update the metadate on the database from a list of updates to do and a list of
-- metadata to remove.
updateMetadata :: (MonadKorrvigs m) => Entry -> Map Text Value -> [Text] -> m ()
updateMetadata entry upd rm = do
  case entry ^. entryKindData of
    FileD file -> File.updateMetadata file upd rm
    NoteD note -> Note.updateMetadata note upd rm
    EventD event -> Event.updateMetadata event upd rm
    CalendarD cal -> Cal.updateMetadata cal upd rm
    SyndicateD syn -> Syn.updateMetadata syn upd rm
  let touchedMetadata = S.fromList $ CI.mk <$> M.keys upd ++ rm
  if S.disjoint idMetadata touchedMetadata
    then updateMetadataSQL entry upd rm
    else syncOne entry

updateMetadataSQL :: (MonadKorrvigs m) => Entry -> Map Text Value -> [Text] -> m ()
updateMetadataSQL entry upd rm = do
  let sqlI = entry ^. entryId
  let rows = mkRow sqlI <$> M.toList upd
  atomicSQL $ \conn -> do
    let todelete = sqlArray sqlStrictText $ rm ++ M.keys upd
    void $
      runDelete conn $
        Delete
          { dTable = entriesMetadataTable,
            dWhere = \mtdt -> sqlElem (mtdt ^. sqlKey) todelete .&& mtdt ^. sqlEntry .== sqlInt4 sqlI,
            dReturning = rCount
          }
    void $
      runInsert conn $
        Insert
          { iTable = entriesMetadataTable,
            iRows = rows,
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  when (any (\m -> CI.mk m `S.member` indexedMetadata) $ M.keys upd ++ rm) $ syncOne entry
  where
    mkRow :: Int -> (Text, Value) -> MetadataRowSQL
    mkRow i (key, val) = MetadataRow (sqlInt4 i) (sqlStrictText key) (sqlValueJSONB val)

updateParents :: (MonadKorrvigs m) => Entry -> [Id] -> [Id] -> m ()
updateParents entry toAdd toRm = do
  let sqlI = entry ^. entryId
  case entry ^. entryKindData of
    FileD file -> File.updateParents file toAdd toRm
    NoteD note -> Note.updateParents note toAdd toRm
    EventD event -> Event.updateParents event toAdd toRm
    CalendarD cal -> Cal.updateParents cal toAdd toRm
    SyndicateD syn -> Syn.updateParents syn toAdd toRm
  rmSql <- fmap catMaybes $ forM toRm $ \rm -> rSelectOne $ do
    e <- selectTable entriesTable
    where_ $ e ^. sqlEntryName .== sqlId rm
    pure $ e ^. sqlEntryId
  rows :: [RelRow] <- fmap catMaybes $ forM toAdd $ \add -> rSelectOne $ do
    e <- selectTable entriesTable
    where_ $ e ^. sqlEntryName .== sqlId add
    pure $ RelRow (sqlInt4 sqlI) (e ^. sqlEntryId)
  atomicSQL $ \conn -> do
    unless (null toRm) $
      void $
        runDelete conn $
          Delete
            { dTable = entriesSubTable,
              dWhere = \sub -> sub ^. source .== sqlInt4 sqlI .&& sqlElem (sub ^. target) (sqlArray sqlInt4 rmSql),
              dReturning = rCount
            }
    unless (null toAdd) $
      void $
        runInsert conn $
          Insert
            { iTable = entriesSubTable,
              iRows = toFields <$> rows,
              iReturning = rCount,
              iOnConflict = Just doNothing
            }

updateDate :: (MonadKorrvigs m) => Entry -> Maybe ZonedTime -> m ()
updateDate entry ntime = do
  let sqlI = entry ^. entryId
  case entry ^. entryKindData of
    FileD file -> File.updateDate file ntime
    NoteD note -> Note.updateDate note ntime
    EventD _ -> undefined
    CalendarD cal -> Cal.updateDate cal ntime
    SyndicateD syn -> Syn.updateDate syn ntime
  atomicSQL $ \conn -> do
    void $
      runUpdate conn $
        Update
          { uTable = entriesTable,
            uUpdateWith = updateEasy $ sqlEntryDate .~ maybe O.null (toNullable . sqlZonedTime) ntime,
            uWhere = \e -> e ^. sqlEntryId .== sqlInt4 sqlI,
            uReturning = rCount
          }

listCompute :: (MonadKorrvigs m) => Id -> m [Text]
listCompute i =
  rSelect $ do
    cmp <- selectTable computationsTable
    name <- nameFor $ cmp ^. sqlCompEntry
    where_ $ name .== sqlId i
    pure $ cmp ^. sqlCompName

updateRefs :: (MonadKorrvigs m) => Entry -> Maybe Id -> m ()
updateRefs oldEntry new = do
  subs <- rSelect $ selectSourcesFor entriesSubTable $ sqlInt4 $ oldEntry ^. entryId
  refs <- rSelect $ selectSourcesFor entriesRefTable $ sqlInt4 $ oldEntry ^. entryId
  forM_ (S.fromList subs <> S.fromList refs) $
    loadSql >=> \case
      Nothing -> pure ()
      Just other -> updateRef other (oldEntry ^. entryName) new
  updateRefSQL oldEntry new

updateRef :: (MonadKorrvigs m) => Entry -> Id -> Maybe Id -> m ()
updateRef entry old new = do
  case entry ^. entryKindData of
    FileD file -> File.updateRef file old new
    NoteD note -> Note.updateRef note old new
    EventD ev -> Event.updateRef ev old new
    CalendarD cal -> Cal.updateRef cal old new
    SyndicateD syn -> Syn.updateRef syn old new

updateRefSQL :: (MonadKorrvigs m) => Entry -> Maybe Id -> m ()
updateRefSQL oldEntry new = do
  let old = oldEntry ^. entryName
  atomicSQL $ \conn -> do
    -- Update sub and ref tables
    when (isNothing new) $ do
      void $
        runDelete conn $
          Delete
            { dTable = entriesRefTable,
              dWhere = \ref -> ref ^. target .== sqlInt4 (oldEntry ^. entryId),
              dReturning = rCount
            }
      void $
        runDelete conn $
          Delete
            { dTable = entriesSubTable,
              dWhere = \ref -> ref ^. target .== sqlInt4 (oldEntry ^. entryId),
              dReturning = rCount
            }
    -- Update collections
    void $ case new of
      Just nw ->
        runUpdate conn $
          Update
            { uTable = notesCollectionsTable,
              uUpdateWith = sqlNoteColEntry .~ sqlId nw,
              uWhere = \colItem -> colItem ^. sqlNoteColEntry .== sqlId old,
              uReturning = rCount
            }
      Nothing ->
        runDelete conn $
          Delete
            { dTable = notesCollectionsTable,
              dWhere = \colItem -> colItem ^. sqlNoteColEntry .== sqlId old,
              dReturning = rCount
            }
    -- Update metadata
    void $ case new of
      Just nw ->
        runUpdate conn $
          Update
            { uTable = entriesMetadataTable,
              uUpdateWith = sqlValue .~ sqlValueJSONB (toJSON $ unId nw),
              uWhere = \mtdt ->
                mtdt
                  ^. sqlValue
                    .== sqlValueJSONB (toJSON $ unId old)
                    .&& in_ idMetadataSQL (mtdt ^. sqlKey),
              uReturning = rCount
            }
      Nothing ->
        runDelete conn $
          Delete
            { dTable = entriesMetadataTable,
              dWhere = \mtdt ->
                mtdt
                  ^. sqlValue
                    .== sqlValueJSONB (toJSON $ unId old)
                    .&& in_ idMetadataSQL (mtdt ^. sqlKey),
              dReturning = rCount
            }
  where
    idMetadataSQL :: [Field SqlText]
    idMetadataSQL = sqlStrictText . CI.foldedCase <$> S.toList idMetadata

updateTitle :: (MonadKorrvigs m) => Entry -> Maybe Text -> m ()
updateTitle entry ntitle = do
  case entry ^. entryKindData of
    FileD file -> File.updateTitle file ntitle
    NoteD note -> Note.updateTitle note ntitle
    EventD ev -> Event.updateTitle ev ntitle
    CalendarD cal -> Cal.updateTitle cal ntitle
    SyndicateD syn -> Syn.updateTitle syn ntitle
  unless (isNothing ntitle && entry ^. kind == Kd.Note) $
    withSQL $ \conn -> do
      void $
        liftIO $
          runUpdate conn $
            Update
              { uTable = entriesTable,
                uUpdateWith = updateEasy $ sqlEntryTitle .~ maybe O.null (toNullable . sqlStrictText) ntitle,
                uWhere = \e -> e ^. sqlEntryId .== sqlInt4 (entry ^. entryId),
                uReturning = rCount
              }
