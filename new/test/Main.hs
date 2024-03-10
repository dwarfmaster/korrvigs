module Main (main) where

import qualified Korrvigs.FTS as FTS
import System.Exit
import Test.HUnit

tests :: Test
tests =
  test
    ["FTS" ~: FTS.tests]

main :: IO ()
main = runTestTT tests >>= \r -> if failures r > 0 then exitFailure else exitSuccess
