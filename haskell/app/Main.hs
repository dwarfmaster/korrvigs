module Main where

import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Korrvigs.Web
import Yesod

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname='korrvigs'"
  warp 3000 $ Korrvigs conn
  close conn
