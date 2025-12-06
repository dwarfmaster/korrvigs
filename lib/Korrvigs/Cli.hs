module Korrvigs.Cli (main) where

import qualified Korrvigs.Cli.ADB as Adb
import qualified Korrvigs.Cli.Compute as Compute
import qualified Korrvigs.Cli.Config as Config
import qualified Korrvigs.Cli.ERIS as Eris
import qualified Korrvigs.Cli.Event as Event
import qualified Korrvigs.Cli.File as File
import qualified Korrvigs.Cli.Import as Import
import qualified Korrvigs.Cli.Info as Info
import Korrvigs.Cli.Monad
import qualified Korrvigs.Cli.Note as Note
import qualified Korrvigs.Cli.Query as Query
import qualified Korrvigs.Cli.Server as Server
import qualified Korrvigs.Cli.Sync as Sync
import Options.Applicative

data Command
  = Info Info.Cmd
  | Note Note.Cmd
  | File File.Cmd
  | Sync Sync.Cmd
  | Query Query.Cmd
  | Config Config.Cmd
  | Server Server.Cmd
  | Event Event.Cmd
  | Compute Compute.Cmd
  | Adb Adb.Cmd
  | Import Import.Cmd
  | Eris Eris.Cmd

parser' :: Parser Command
parser' =
  subparser $
    command "info" (Info <$> Info.parser)
      <> command "note" (Note <$> Note.parser)
      <> command "file" (File <$> File.parser)
      <> command "sync" (Sync <$> Sync.parser)
      <> command "query" (Query <$> Query.parser)
      <> command "config" (Config <$> Config.parser)
      <> command "server" (Server <$> Server.parser)
      <> command "event" (Event <$> Event.parser)
      <> command "compute" (Compute <$> Compute.parser)
      <> command "adb" (Adb <$> Adb.parser)
      <> command "import" (Import <$> Import.parser)
      <> command "eris" (Eris <$> Eris.parser)

parser :: ParserInfo Command
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "CLI interface for Korrvigs knowledge database"
      <> header "korr -- interface for Korrvigs"

run :: Command -> KorrM ()
run (Info cmd) = Info.run cmd
run (Note cmd) = Note.run cmd
run (File cmd) = File.run cmd
run (Sync cmd) = Sync.run cmd
run (Query cmd) = Query.run cmd
run (Config cmd) = Config.run cmd
run (Server cmd) = Server.run cmd
run (Event cmd) = Event.run cmd
run (Compute cmd) = Compute.run cmd
run (Adb cmd) = Adb.run cmd
run (Import cmd) = Import.run cmd
run (Eris cmd) = Eris.run cmd

main :: IO ()
main = do
  cmd <- execParser parser
  r <- runKorrMWithConfig $ run cmd
  case r of
    Left err -> print err
    Right () -> pure ()
