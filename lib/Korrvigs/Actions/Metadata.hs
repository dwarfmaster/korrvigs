module Korrvigs.Actions.Metadata (updateMetadata) where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.AllEntries ()
import Korrvigs.Entry
import Korrvigs.KindData
import Korrvigs.Monad
import Opaleye hiding (not, null)

-- Update the metadate on the database from a list of updates to do and a list of
-- metadata to remove.
updateMetadata :: (MonadKorrvigs m) => Entry -> Map Text Value -> [Text] -> m ()
updateMetadata entry upd rm = do
  let i = entry ^. name
  case entry ^. kindData of
    LinkD link -> dUpdateMetadata link upd rm
    FileD file -> dUpdateMetadata file upd rm
    NoteD note -> dUpdateMetadata note upd rm
    EventD event -> dUpdateMetadata event upd rm
  let rows = mkRow i <$> M.toList upd
  atomicSQL $ \conn -> do
    void $
      runDelete conn $
        Delete
          { dTable = entriesMetadataTable,
            dWhere = \mtdt -> sqlElem (mtdt ^. sqlKey) (sqlArray sqlStrictText rm) .&& mtdt ^. sqlEntry .== sqlId i,
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
  pure ()
  where
    mkRow :: Id -> (Text, Value) -> MetadataRowSQL
    mkRow i (key, val) = MetadataRow (sqlId i) (sqlStrictText key) (sqlValueJSONB val)
