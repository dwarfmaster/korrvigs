module Korrvigs.Cli where

import qualified Korrvigs.Cli.Info as Info
import Korrvigs.Cli.Monad
import Options.Applicative

newtype Command = Info Info.Cmd

parser' :: Parser Command
parser' =
  subparser $
    command "info" (Info <$> Info.parser)

parser :: ParserInfo Command
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "CLI interface for Korrvigs knowledge database"
      <> header "korr -- interface for Korrvigs"

run :: Command -> KorrM ()
run (Info inf) = Info.run inf
