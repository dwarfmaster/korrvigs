module Korrvigs.KindData where

import Data.Set (Set)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad

class IsKD a where
  -- Load from database
  dLoad :: MonadKorrvigs m => Id -> ((Entry -> a) -> Entry) -> m (Maybe Entry)

  -- Add a new entry
  data KDMaker a
  dMake :: MonadKorrvigs m => KDMaker a -> m ()

  -- Update an entry
  data KDUpdater a
  dUpdate :: MonadKorrvigs m => KDUpdater a -> m ()

  -- List the ids present in the filesystem
  data KDIdentifier a
  dList :: m (Set (KDIdentifier a))
  dGetId :: KDIdentifier a -> Id

  -- Sync the content of the filesystem to the database
  dSync :: MonadKorrvigs m => f a -> m ()
  dSyncOne :: MonadKorrvigs m => KDIdentifier a -> m ()

  -- Remove from all the tables and the filesystem, including the entries table
  dRemove :: MonadKorrvigs m => KDIdentifier a -> m ()

  -- Query basic information
  dKind :: a -> Kind
  dEntry :: a -> Entry
  dIdentify :: a -> KDIdentifier a
  dToData :: a -> KindData
