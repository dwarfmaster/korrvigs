module Korrvigs.Web.Backend where

import Database.PostgreSQL.Simple (Connection)
import Yesod (HandlerFor, getYesod)

class Backend a where
  backendSqlConnection :: a -> Connection

pgsql :: Backend a => HandlerFor a Connection
pgsql = getYesod >>= return . backendSqlConnection
