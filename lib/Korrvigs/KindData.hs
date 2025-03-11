module Korrvigs.KindData where

import Control.Lens
import Data.Aeson (Value)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Monad

data RelData = RelData
  { _relSubOf :: [Id],
    _relRefTo :: [Id]
  }

makeLenses ''RelData

class IsKD a where
  -- List the ids present in the filesystem
  data KDIdentifier a
  dList :: (MonadKorrvigs m) => f a -> m (Set (KDIdentifier a))
  dGetId :: KDIdentifier a -> Id

  -- Computation
  dListCompute :: (MonadKorrvigs m) => a -> m EntryComps

  -- Sync the content of the filesystem to the database and extract the relation data
  dSync :: (MonadKorrvigs m) => f a -> m (Map Id (RelData, EntryComps))
  dSyncOne :: (MonadKorrvigs m) => KDIdentifier a -> m (RelData, EntryComps)

  -- Update metadata in the filesystem only
  -- The first argument are metadata keys to be updated/inserted, and the second one metadata keys
  -- to be removed. It is assumed the removal happen first.
  dUpdateMetadata :: (MonadKorrvigs m) => a -> Map Text Value -> [Text] -> m ()
  dUpdateParents :: (MonadKorrvigs m) => a -> [Id] -> [Id] -> m ()

atomicInsertRelData :: (MonadKorrvigs m) => Id -> RelData -> m ()
atomicInsertRelData src relData =
  atomicInsert $
    insertSubOf ((src,) <$> relData ^. relSubOf)
      <> insertRefTo ((src,) <$> relData ^. relRefTo)
