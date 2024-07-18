{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Korrvigs.Cli.Monad where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import qualified Korrvigs.Actions as Actions
import Korrvigs.Monad

data KorrState = KState
  { _korrConnection :: Connection,
    _korrRoot :: FilePath
  }

makeLenses ''KorrState

newtype KorrM a = KorrM (ExceptT KorrvigsError (ReaderT KorrState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader KorrState, MonadError KorrvigsError)

instance MonadKorrvigs KorrM where
  pgSQL = view korrConnection
  root = view korrRoot
  load = Actions.load
  remove = Actions.remove
  dispatchRemove = Actions.dispatchRemove
  removeDB = Actions.removeDB
  dispatchRemoveDB = Actions.dispatchRemoveDB
  sync = Actions.sync

runKorrM :: ByteString -> KorrM a -> IO (Either KorrvigsError a)
runKorrM connSpec (KorrM action) = do
  conn <- connectPostgreSQL connSpec
  r <- runReaderT (runExceptT action) $ KState conn "/tmp/korrvigs"
  close conn
  pure r
