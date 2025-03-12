module Korrvigs.KindData where

import Control.Lens
import Data.Map (Map)
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Monad

data RelData = RelData
  { _relSubOf :: [Id],
    _relRefTo :: [Id]
  }

makeLenses ''RelData

class IsKD a where
  data KDIdentifier a

  -- Sync the content of the filesystem to the database and extract the relation data
  dSync :: (MonadKorrvigs m) => f a -> m (Map Id (RelData, EntryComps))
  dSyncOne :: (MonadKorrvigs m) => KDIdentifier a -> m (RelData, EntryComps)
