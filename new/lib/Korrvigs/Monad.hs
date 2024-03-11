module Korrvigs.Monad where

import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Connection)

class MonadIO m => MonadKorrvigs m where
  pgSQL :: m Connection
  root :: m FilePath
