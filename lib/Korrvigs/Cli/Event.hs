module Korrvigs.Cli.Event where

import Conduit
import Control.Lens hiding (argument)
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import qualified Korrvigs.Calendar.DAV as DAV
import qualified Korrvigs.Calendar.New as NC
import Korrvigs.Calendar.SQL
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New
import Korrvigs.Entry
import Korrvigs.Event.New
import Korrvigs.Monad
import Korrvigs.Utils.DateParser (dayParser)
import Korrvigs.Utils.Time (measureTime_)
import Opaleye (selectTable)
import Options.Applicative
import System.IO hiding (putStrLn, utf8)
import Prelude hiding (putStrLn)

data Cmd
  = Sync
  | New NewEvent
  | NewCal NC.NewCalendar
  | Pull

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
            (pure Pull)
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

listCalendars :: KorrM [Calendar]
listCalendars = do
  calIds <- rSelect $ view sqlCalName <$> selectTable calendarsTable
  calEntries <- mapM load calIds
  let toCal entry = case entry ^. kindData of
        CalendarD cal -> Just cal
        _ -> Nothing
  pure $ mapMaybe (>>= toCal) calEntries

getPwd :: KorrM Text
getPwd = do
  liftIO $ putStr "Password: "
  liftIO $ hFlush stdout
  pwd <- liftIO $ T.pack <$> withEcho False getLine
  liftIO $ putStrLn ""
  pure pwd

run :: Cmd -> KorrM ()
run Sync = do
  cals <- listCalendars
  pwd <- getPwd
  DAV.sync False cals pwd
run (New nevent) = do
  i <- new nevent
  liftIO $ putStrLn $ unId i
run (NewCal ncal) = do
  i <- NC.new ncal
  liftIO $ putStrLn $ unId i
run Pull = do
  cals <- listCalendars
  pwd <- getPwd
  forM_ cals $ \cal -> do
    txt <- measureTime_ $ DAV.pull cal pwd
    liftIO $ putStrLn $ "Pulled from calendar " <> unId (cal ^. calEntry . name) <> " in " <> txt

-- Caldav
withEcho :: Bool -> IO a -> IO a
withEcho echo act = do
  old <- hGetEcho stdin
  hSetEcho stdin echo
  r <- act
  hSetEcho stdin old
  pure r
