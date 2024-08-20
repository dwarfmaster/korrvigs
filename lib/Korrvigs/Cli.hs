module Korrvigs.Cli (main) where

import qualified Korrvigs.Cli.File as File
import qualified Korrvigs.Cli.Info as Info
import qualified Korrvigs.Cli.Link as Link
import Korrvigs.Cli.Monad
import qualified Korrvigs.Cli.Note as Note
import qualified Korrvigs.Cli.Sync as Sync
import Options.Applicative

data Command
  = Info Info.Cmd
  | Link Link.Cmd
  | Note Note.Cmd
  | File File.Cmd
  | Sync Sync.Cmd

parser' :: Parser Command
parser' =
  subparser $
    command "info" (Info <$> Info.parser)
      <> command "link" (Link <$> Link.parser)
      <> command "note" (Note <$> Note.parser)
      <> command "file" (File <$> File.parser)
      <> command "sync" (Sync <$> Sync.parser)

parser :: ParserInfo Command
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "CLI interface for Korrvigs knowledge database"
      <> header "korr -- interface for Korrvigs"

run :: Command -> KorrM ()
run (Info cmd) = Info.run cmd
run (Link cmd) = Link.run cmd
run (Note cmd) = Note.run cmd
run (File cmd) = File.run cmd
run (Sync cmd) = Sync.run cmd

main :: IO ()
main = do
  cmd <- execParser parser
  r <- runKorrM "dbname='korrvigs_new'" (run cmd)
  case r of
    Left err -> print err
    Right () -> pure ()
