module Korrvigs.Monad.Metadata
  ( updateMetadata,
    updateParents,
    updateDate,
    listCompute,
  )
where

import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time.LocalTime
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Compute.Action
import Korrvigs.Entry
import qualified Korrvigs.Event.Sync as Event
import qualified Korrvigs.File.Sync as File
import qualified Korrvigs.Link.Sync as Link
import Korrvigs.Monad.Class
import Korrvigs.Monad.SQL (indexedMetadata)
import Korrvigs.Monad.Sync (syncOne)
import qualified Korrvigs.Note.Sync as Note
import Opaleye hiding (not, null)
import qualified Opaleye as O

-- Update the metadate on the database from a list of updates to do and a list of
-- metadata to remove.
updateMetadata :: (MonadKorrvigs m) => Entry -> Map Text Value -> [Text] -> m ()
updateMetadata entry upd rm = do
  let sqlI = entry ^. entryId
  case entry ^. entryKindData of
    LinkD link -> Link.updateMetadata link upd rm
    FileD file -> File.updateMetadata file upd rm
    NoteD note -> Note.updateMetadata note upd rm
    EventD event -> Event.updateMetadata event upd rm
    CalendarD cal -> Cal.updateMetadata cal upd rm
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
    LinkD link -> Link.updateParents link toAdd toRm
    FileD file -> File.updateParents file toAdd toRm
    NoteD note -> Note.updateParents note toAdd toRm
    EventD event -> Event.updateParents event toAdd toRm
    CalendarD cal -> Cal.updateParents cal toAdd toRm
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
    LinkD link -> Link.updateDate link ntime
    FileD file -> File.updateDate file ntime
    NoteD note -> Note.updateDate note ntime
    EventD _ -> undefined
    CalendarD cal -> Cal.updateDate cal ntime
  atomicSQL $ \conn -> do
    void $
      runUpdate conn $
        Update
          { uTable = entriesTable,
            uUpdateWith = updateEasy $ sqlEntryDate .~ maybe O.null (toNullable . sqlZonedTime) ntime,
            uWhere = \e -> e ^. sqlEntryId .== sqlInt4 sqlI,
            uReturning = rCount
          }

listCompute :: (MonadKorrvigs m) => Id -> m (Map Text Action)
listCompute i = do
  acts <- rSelect $ do
    cmp <- selectTable computationsTable
    name <- nameFor $ cmp ^. sqlCompEntry
    where_ $ name .== sqlId i
    pure (cmp ^. sqlCompName, cmp ^. sqlCompAction)
  pure $ M.fromList acts
