module Korrvigs.Cli (main) where

import qualified Korrvigs.Cli.ADB as Adb
import qualified Korrvigs.Cli.Collections as Cols
import qualified Korrvigs.Cli.Compute as Compute
import qualified Korrvigs.Cli.Config as Config
import qualified Korrvigs.Cli.Event as Event
import qualified Korrvigs.Cli.File as File
import qualified Korrvigs.Cli.Info as Info
import qualified Korrvigs.Cli.Init as Init
import qualified Korrvigs.Cli.Link as Link
import Korrvigs.Cli.Monad
import qualified Korrvigs.Cli.Note as Note
import qualified Korrvigs.Cli.Query as Query
import qualified Korrvigs.Cli.Server as Server
import qualified Korrvigs.Cli.Sync as Sync
import Options.Applicative

data KorrCommand
  = Info Info.Cmd
  | Link Link.Cmd
  | Note Note.Cmd
  | File File.Cmd
  | Sync Sync.Cmd
  | Query Query.Cmd
  | Config Config.Cmd
  | Server Server.Cmd
  | Event Event.Cmd
  | Compute Compute.Cmd
  | Collections Cols.Cmd
  | Adb Adb.Cmd

data Command
  = Init Init.Cmd
  | KCmd KorrCommand

parser' :: Parser Command
parser' =
  subparser $
    command "info" (KCmd . Info <$> Info.parser)
      <> command "link" (KCmd . Link <$> Link.parser)
      <> command "note" (KCmd . Note <$> Note.parser)
      <> command "file" (KCmd . File <$> File.parser)
      <> command "sync" (KCmd . Sync <$> Sync.parser)
      <> command "query" (KCmd . Query <$> Query.parser)
      <> command "config" (KCmd . Config <$> Config.parser)
      <> command "server" (KCmd . Server <$> Server.parser)
      <> command "event" (KCmd . Event <$> Event.parser)
      <> command "compute" (KCmd . Compute <$> Compute.parser)
      <> command "collections" (KCmd . Collections <$> Cols.parser)
      <> command "adb" (KCmd . Adb <$> Adb.parser)
      <> command "init" (Init <$> Init.parser)

parser :: ParserInfo Command
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "CLI interface for Korrvigs knowledge database"
      <> header "korr -- interface for Korrvigs"

run :: KorrCommand -> KorrM ()
run (Info cmd) = Info.run cmd
run (Link cmd) = Link.run cmd
run (Note cmd) = Note.run cmd
run (File cmd) = File.run cmd
run (Sync cmd) = Sync.run cmd
run (Query cmd) = Query.run cmd
run (Config cmd) = Config.run cmd
run (Server cmd) = Server.run cmd
run (Event cmd) = Event.run cmd
run (Compute cmd) = Compute.run cmd
run (Collections cmd) = Cols.run cmd
run (Adb cmd) = Adb.run cmd

main :: IO ()
main = do
  cmd <- execParser parser
  case cmd of
    Init icmd -> Init.run icmd
    KCmd kcmd -> do
      r <- runKorrMWithConfig $ run kcmd
      case r of
        Left err -> print err
        Right () -> pure ()
