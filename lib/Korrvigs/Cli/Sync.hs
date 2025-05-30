module Korrvigs.Cli.Sync where

import Control.Lens
import Korrvigs.Cli.Monad
import qualified Korrvigs.Monad.Sync as A
import Options.Applicative

newtype Cmd = Cmd
  {_verbose :: Bool}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  Cmd <$> switch (long "verbose" <> short 'v' <> help "Display more information during syncing")

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Sync database to filesystem content"
      <> header "korr sync -- Synchronize"

run :: Cmd -> KorrM ()
run _ = A.sync
