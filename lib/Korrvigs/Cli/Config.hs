module Korrvigs.Cli.Config where

import Control.Monad.IO.Class
import Korrvigs.Cli.Monad
import Korrvigs.Monad
import Options.Applicative

data Cmd = GetRoot

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "root"
      ( info
          (pure GetRoot <**> helper)
          ( progDesc "Get korrvigs root from config"
              <> header "korr config root"
          )
      )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Get config options"
      <> header "korr config -- Access config"

run :: Cmd -> KorrM ()
run GetRoot = root >>= liftIO . putStrLn
