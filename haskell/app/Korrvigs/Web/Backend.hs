module Korrvigs.Web.Backend where

import Database.PostgreSQL.Simple (Connection)
import Yesod (HandlerFor)

data Korrvigs = Korrvigs Connection

type HM a = HandlerFor Korrvigs a
