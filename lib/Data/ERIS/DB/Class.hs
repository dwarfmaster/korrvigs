module Data.ERIS.DB.Class where

import Data.ERIS.Crypto

class (Monad m) => ERISBlockRead db m where
  erisBlockStorageGet :: db -> ERISHash -> m (Maybe ERISBlock)

class (ERISBlockRead db m) => ERISBlockWrite db m where
  erisBlockStoragePut :: db -> ERISHash -> ERISBlock -> m ()
