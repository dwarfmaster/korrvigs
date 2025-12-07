{-# LANGUAGE UndecidableInstances #-}

module Data.ERIS.DB.Class where

import Conduit
import Control.Monad.RWS.Lazy
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ERIS.Crypto
import Data.Map (Map)
import qualified Data.Map as M

-- Reader
class (Monad m) => ERISBlockRead db m where
  erisBlockStorageGet :: db -> ERISHash -> m (Maybe ERISBlock)

instance (ERISBlockRead db m) => ERISBlockRead db (ReaderT a m) where
  erisBlockStorageGet db hsh = lift $ erisBlockStorageGet db hsh

instance (Monoid a, ERISBlockRead db m) => ERISBlockRead db (WriterT a m) where
  erisBlockStorageGet db hsh = lift $ erisBlockStorageGet db hsh

instance (ERISBlockRead db m) => ERISBlockRead db (StateT a m) where
  erisBlockStorageGet db hsh = lift $ erisBlockStorageGet db hsh

instance (Monoid w, ERISBlockRead db m) => ERISBlockRead db (RWST r w s m) where
  erisBlockStorageGet db hsh = lift $ erisBlockStorageGet db hsh

instance (ERISBlockRead db m) => ERISBlockRead db (ConduitT i o m) where
  erisBlockStorageGet db hsh = lift $ erisBlockStorageGet db hsh

-- Writer
class (Monad m) => ERISBlockWrite db m where
  erisBlockStoragePut :: db -> ERISHash -> ERISBlock -> m ()

instance (ERISBlockWrite db m) => ERISBlockWrite db (ReaderT a m) where
  erisBlockStoragePut db hsh = lift . erisBlockStoragePut db hsh

instance (Monoid a, ERISBlockWrite db m) => ERISBlockWrite db (WriterT a m) where
  erisBlockStoragePut db hsh = lift . erisBlockStoragePut db hsh

instance (ERISBlockWrite db m) => ERISBlockWrite db (StateT a m) where
  erisBlockStoragePut db hsh = lift . erisBlockStoragePut db hsh

instance (Monoid w, ERISBlockWrite db m) => ERISBlockWrite db (RWST r w s m) where
  erisBlockStoragePut db hsh = lift . erisBlockStoragePut db hsh

instance (ERISBlockWrite db m) => ERISBlockWrite db (ConduitT i o m) where
  erisBlockStoragePut db hsh = lift . erisBlockStoragePut db hsh

-- In memory
data ERISInMemoryDB = ERISInMemoryDB

type InMemoryDB = Map ERISHash ERISBlock

instance (Monad m) => ERISBlockRead ERISInMemoryDB (StateT InMemoryDB m) where
  erisBlockStorageGet ERISInMemoryDB hsh = gets $ M.lookup hsh

instance (Monad m) => ERISBlockWrite ERISInMemoryDB (StateT InMemoryDB m) where
  erisBlockStoragePut ERISInMemoryDB hsh block =
    modify $ M.insert hsh block
