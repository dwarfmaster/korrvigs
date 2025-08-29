module Korrvigs.Monad.Metadata (updateMetadata, updateParents, listCompute) where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Compute.Action
import Korrvigs.Entry
import qualified Korrvigs.Event.Sync as Event
import qualified Korrvigs.File.Sync as File
import qualified Korrvigs.Link.Sync as Link
import Korrvigs.Monad.Class
import qualified Korrvigs.Note.Sync as Note
import Opaleye hiding (not, null)

-- Update the metadate on the database from a list of updates to do and a list of
-- metadata to remove.
updateMetadata :: (MonadKorrvigs m) => Entry -> Map Text Value -> [Text] -> m ()
updateMetadata entry upd rm = do
  let i = entry ^. name
  case entry ^. kindData of
    LinkD link -> Link.updateMetadata link upd rm
    FileD file -> File.updateMetadata file upd rm
    NoteD note -> Note.updateMetadata note upd rm
    EventD event -> Event.updateMetadata event upd rm
    CalendarD cal -> Cal.updateMetadata cal upd rm
  let rows = mkRow i <$> M.toList upd
  atomicSQL $ \conn -> do
    let todelete = sqlArray sqlStrictText $ rm ++ M.keys upd
    void $
      runDelete conn $
        Delete
          { dTable = entriesMetadataTable,
            dWhere = \mtdt -> sqlElem (mtdt ^. sqlKey) todelete .&& mtdt ^. sqlEntry .== sqlId i,
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
  where
    mkRow :: Id -> (Text, Value) -> MetadataRowSQL
    mkRow i (key, val) = MetadataRow (sqlId i) (sqlStrictText key) (sqlValueJSONB val)

updateParents :: (MonadKorrvigs m) => Entry -> [Id] -> [Id] -> m ()
updateParents entry toAdd toRm = do
  let i = entry ^. name
  case entry ^. kindData of
    LinkD link -> Link.updateParents link toAdd toRm
    FileD file -> File.updateParents file toAdd toRm
    NoteD note -> Note.updateParents note toAdd toRm
    EventD event -> Event.updateParents event toAdd toRm
    CalendarD cal -> Cal.updateParents cal toAdd toRm
  let rows = RelRow i <$> toAdd
  atomicSQL $ \conn -> do
    unless (null toRm) $
      void $
        runDelete conn $
          Delete
            { dTable = entriesSubTable,
              dWhere = \sub -> sub ^. source .== sqlId i .&& sqlElem (sub ^. target) (sqlArray sqlId toRm),
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

listCompute :: (MonadKorrvigs m) => Id -> m (Map Text Action)
listCompute i = do
  acts <- rSelect $ do
    cmp <- selectTable computationsTable
    where_ $ cmp ^. sqlCompEntry .== sqlId i
    pure (cmp ^. sqlCompName, cmp ^. sqlCompAction)
  pure $ M.fromList acts
