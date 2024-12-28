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

-- Returns the updated entry on success, and the list of read-only metadatas that were
-- attempted to be changed otherwise.
updateMetadata :: (MonadKorrvigs m) => Entry -> Map Text Value -> [Text] -> m (Either [Text] Entry)
updateMetadata entry upd rm = do
  let i = entry ^. name
  let mtdtKeys = (fst <$> M.toList upd) ++ rm
  ros <- rSelect $ do
    mtdt <- selectTable entriesMetadataTable
    where_ $ mtdt ^. sqlEntry .== sqlId i
    where_ $ sqlElem (mtdt ^. sqlKey) $ sqlArray sqlStrictText mtdtKeys
    where_ $ mtdt ^. sqlReadOnly .== sqlBool True
    pure $ mtdt ^. sqlKey
  if not (null ros)
    then pure $ Left ros
    else do
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
      pure $ Right $ entry & metadata %~ updateMetadataMap upd rm
  where
    mkRow :: Id -> (Text, Value) -> MetadataRowSQL
    mkRow i (key, val) = MetadataRow (sqlId i) (sqlStrictText key) (sqlValueJSONB val) (sqlBool False)

updateMetadataMap :: Map Text Value -> [Text] -> Metadata -> Metadata
updateMetadataMap upd rm =
  foldr (.) id $
    ((\(k, v) -> M.insert k (MValue v False)) <$> M.toList upd) ++ (M.delete <$> rm)
