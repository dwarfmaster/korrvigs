module Korrvigs.Cli (main) where

import qualified Korrvigs.Cli.Info as Info
import qualified Korrvigs.Cli.Link as Link
import Korrvigs.Cli.Monad
import Options.Applicative

data Command
  = Info Info.Cmd
  | Link Link.Cmd

parser' :: Parser Command
parser' =
  subparser $
    command "info" (Info <$> Info.parser)
      <> command "link" (Link <$> Link.parser)

parser :: ParserInfo Command
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "CLI interface for Korrvigs knowledge database"
      <> header "korr -- interface for Korrvigs"

run :: Command -> KorrM ()
run (Info cmd) = Info.run cmd
run (Link cmd) = Link.run cmd

main :: IO ()
main = do
  cmd <- execParser parser
  r <- runKorrM "dbname='korrvigs_new'" (run cmd)
  case r of
    Left err -> print err
    Right () -> pure ()
