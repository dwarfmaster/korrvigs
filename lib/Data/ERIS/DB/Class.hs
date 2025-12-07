{-# LANGUAGE UndecidableInstances #-}

module Data.ERIS.DB.Class where

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.ERIS.Crypto
import Data.Map (Map)
import qualified Data.Map as M

class (Monad m) => ERISBlockRead db m where
  erisBlockStorageGet :: db -> ERISHash -> m (Maybe ERISBlock)

class (Monad m) => ERISBlockWrite db m where
  erisBlockStoragePut :: db -> ERISHash -> ERISBlock -> m ()

data ERISInMemoryDB = ERISInMemoryDB

instance (Monad m, MonadReader (Map ERISHash ERISBlock) m) => ERISBlockRead ERISInMemoryDB m where
  erisBlockStorageGet ERISInMemoryDB hsh =
    M.lookup hsh <$> ask

instance (Monad m, MonadState (Map ERISHash ERISBlock) m) => ERISBlockWrite ERISInMemoryDB m where
  erisBlockStoragePut ERISInMemoryDB hsh block =
    modify $ M.insert hsh block
