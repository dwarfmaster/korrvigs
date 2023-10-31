module Main where

import Korrvigs.Web
import Yesod

main :: IO ()
main = warp 3000 Korrvigs
