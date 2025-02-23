module Korrvigs.Cli.Event where

import Conduit
import Control.Lens hiding (argument)
import Control.Monad
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import qualified Korrvigs.Calendar.DAV as DAV
import qualified Korrvigs.Calendar.New as NC
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New
import Korrvigs.Entry
import Korrvigs.Event.New
import Korrvigs.Event.VDirSyncer
import Korrvigs.Monad
import Korrvigs.Utils.DateParser (dayParser)
import Options.Applicative
import System.FilePath
import System.IO hiding (putStrLn, utf8)
import Prelude hiding (putStrLn)

data Cmd
  = Sync
  | New NewEvent
  | NewCal NC.NewCalendar
  | Pull Text

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "sync"
      ( info
          (pure Sync <**> helper)
          ( progDesc "Sync events with nextcloud"
              <> header "korr event sync -- sync events"
          )
      )
      <> command
        "new"
        ( info
            ( ( New
                  <$> ( NewEvent
                          <$> newEntryOptions
                          <*> (MkId <$> argument str (metavar "CALENDAR" <> help "The calendar to create the event in"))
                          <*> argument dayParser (metavar "START" <> help "The start date of the event, in ISO8601 format")
                          <*> argument dayParser (metavar "END" <> help "The end date of the event, in ISO8601 format")
                          <*> argument str (metavar "SUMMARY" <> help "The summary of the event")
                          <*> optional (option str $ long "description" <> metavar "DESCRIPTION" <> help "The description of the event")
                          <*> optional (option str $ long "location" <> metavar "LOCATION" <> help "The location of the event")
                          <*> flag True False (long "transparent" <> help "Mark the event of transparent busy-ness")
                      )
              )
                <**> helper
            )
            ( progDesc "Create new event"
                <> header "korr event new -- Create event"
            )
        )
      <> command
        "newcal"
        ( info
            ( ( NewCal
                  <$> ( NC.NewCalendar
                          <$> newEntryOptions
                          <*> argument str (metavar "SERVER" <> help "The server to connect")
                          <*> argument str (metavar "USER" <> help "The user of the server")
                          <*> argument str (metavar "CALENDAR" <> help "The name of the calendar in the server")
                      )
              )
                <**> helper
            )
            ( progDesc "Create new calendar"
                <> header "korr event newcal -- Create calendar"
            )
        )
      <> command
        "pull"
        ( info
            (Pull <$> argument str (metavar "CALENDAR"))
            ( progDesc "Pull from caldav server"
                <> header "korr event pull -- Pull events"
            )
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with event entries in Korrvigs"
      <> header "korr event -- interface for events"

run :: Cmd -> KorrM ()
run Sync =
  vdirSync >>= \case
    VDirtyRepo -> liftIO $ putStrLn "Cannot sync, there are uncommited changed to the repo"
    VDirNothingToDo -> liftIO $ putStrLn "Already up to date"
    VDirMergeFailed -> liftIO $ putStrLn "Merge failed"
    VDirMerged -> liftIO $ putStrLn "Successfully synced"
    VDirError err -> liftIO $ putStrLn $ "Unexpected error: " <> err
run (New nevent) = do
  i <- new nevent
  liftIO $ putStrLn $ unId i
run (NewCal ncal) = do
  i <- NC.new ncal
  liftIO $ putStrLn $ unId i
run (Pull calId) = do
  calE <- load (MkId calId) >>= throwMaybe (KMiscError $ "Couldn't find calendar \"" <> calId <> "\"")
  case calE ^. kindData of
    CalendarD cal -> do
      liftIO $ putStr "Password: "
      liftIO $ hFlush stdout
      pwd <- liftIO $ T.pack <$> withEcho False getLine
      changes <- DAV.checkChanges cal pwd Nothing M.empty
      rt <- root
      let worktreeRoot = joinPath [rt, "../../korrvigs-temp/calsync/korrvigs/events"]
      DAV.doPull cal pwd worktreeRoot changes
    _ -> throwM $ KMiscError $ "\"" <> calId <> "\" is not the ID of a calendar"

-- Caldav
withEcho :: Bool -> IO a -> IO a
withEcho echo act = do
  old <- hGetEcho stdin
  hSetEcho stdin echo
  r <- act
  hSetEcho stdin old
  pure r
