module Korrvigs.KindData where

import Control.Lens.TH (makeLenses)
import Data.Map (Map)
import Data.Set (Set)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad

data RelData = RelData
  { _relSubOf :: [Id],
    _relRefTo :: [Id]
  }

makeLenses ''RelData

class IsKD a where
  -- Load/remove from database only
  dLoad :: (MonadKorrvigs m) => Id -> ((Entry -> a) -> Entry) -> m (Maybe Entry)
  dRemoveDB :: (MonadKorrvigs m) => f a -> Id -> m ()

  -- List the ids present in the filesystem
  data KDIdentifier a
  dList :: (MonadKorrvigs m) => f a -> m (Set (KDIdentifier a))
  dGetId :: KDIdentifier a -> Id

  -- Sync the content of the filesystem to the database and extract the relation data
  dSync :: (MonadKorrvigs m) => f a -> m (Map Id RelData)
  dSyncOne :: (MonadKorrvigs m) => KDIdentifier a -> m RelData

  -- Remove from all the tables and the filesystem, including the entries table
  dRemove :: (MonadKorrvigs m) => KDIdentifier a -> m ()

  -- Query basic information
  dKind :: a -> Kind
  dEntry :: a -> Entry
  dIdentify :: a -> KDIdentifier a
  dToData :: a -> KindData
