module Korrvigs.KindData where

import Control.Lens
import Data.Aeson (Value)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Int (Int64)
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Opaleye (Delete)

data RelData = RelData
  { _relSubOf :: [Id],
    _relRefTo :: [Id]
  }

makeLenses ''RelData

class IsKD a where
  -- Load/remove from database only
  dLoad :: (MonadKorrvigs m) => Id -> ((Entry -> a) -> Entry) -> m (Maybe Entry)
  dRemoveDB :: f a -> Id -> [Delete Int64]

  -- List the ids present in the filesystem
  data KDIdentifier a
  dList :: (MonadKorrvigs m) => f a -> m (Set (KDIdentifier a))
  dGetId :: KDIdentifier a -> Id

  -- Computation
  dListCompute :: (MonadKorrvigs m) => KDIdentifier a -> m EntryComps

  -- Sync the content of the filesystem to the database and extract the relation data
  dSync :: (MonadKorrvigs m) => f a -> m (Map Id (RelData, EntryComps))
  dSyncOne :: (MonadKorrvigs m) => KDIdentifier a -> m (RelData, EntryComps)

  -- Remove from the filesystem only, must be called in addtion to dRemoveDB
  dRemove :: (MonadKorrvigs m) => KDIdentifier a -> m ()

  -- Update metadata in the filesystem only
  -- The first argument are metadata keys to be updated/inserted, and the second one metadata keys
  -- to be removed. It is assumed the removal happen first.
  dUpdateMetadata :: (MonadKorrvigs m) => a -> Map Text Value -> [Text] -> m ()
  dUpdateParents :: (MonadKorrvigs m) => a -> [Id] -> [Id] -> m ()

  -- Query basic information
  dKind :: f a -> Kind
  dEntry :: a -> Entry
  dIdentify :: a -> KDIdentifier a
  dToData :: a -> KindData

atomicInsertRelData :: (MonadKorrvigs m) => Id -> RelData -> m ()
atomicInsertRelData src relData =
  atomicInsert $
    insertSubOf ((src,) <$> relData ^. relSubOf)
      <> insertRefTo ((src,) <$> relData ^. relRefTo)
