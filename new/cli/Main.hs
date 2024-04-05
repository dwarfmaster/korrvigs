module Main where

import Korrvigs.Cli
import Korrvigs.Cli.Monad
import Options.Applicative

main :: IO ()
main = do
  cmd <- execParser parser
  r <- runKorrM "dbname='korrvigs_new'" (run cmd)
  case r of
    Left err -> print err
    Right () -> pure ()
