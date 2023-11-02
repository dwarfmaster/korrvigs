module Main where

import Data.Text (Text)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Korrvigs.Web
import Yesod

theme :: Int -> Text
theme 0 = "#231e18"
theme 1 = "#302b25"
theme 2 = "#48413a"
theme 3 = "#9d8b70"
theme 4 = "#b4a490"
theme 5 = "#cabcb1"
theme 6 = "#d7c8bc"
theme 7 = "#e4d4c8"
theme 8 = "#d35c5c"
theme 9 = "#ca7f32"
theme 10 = "#e0ac16"
theme 11 = "#b7ba53"
theme 12 = "#6eb958"
theme 13 = "#88a4d3"
theme 14 = "#bb90e2"
theme 15 = "b49368"
theme _ = error "Unsupported base16 color"

root :: FilePath
root = "../wiki"

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname='korrvigs'"
  warp 3000 $ Korrvigs conn root theme
  close conn
