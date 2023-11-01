module Main where

import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Korrvigs.Web ()
import Korrvigs.Web.Backend
import Yesod

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname='korrvigs'"
  warp 3000 $ Korrvigs conn
  close conn
