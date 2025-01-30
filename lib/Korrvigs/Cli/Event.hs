module Korrvigs.Cli.Event where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.IO (putStrLn)
import Korrvigs.Actions (processRelData)
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New
import Korrvigs.Entry
import Korrvigs.Event.New
import Korrvigs.Event.Sync
import Korrvigs.Event.VDirSyncer
import Korrvigs.Utils.DateParser (dayParser)
import Options.Applicative
import Prelude hiding (putStrLn)

data Cmd
  = Register {_regSilent :: Bool, _regJSON :: Bool}
  | Sync
  | New NewEvent

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "register"
      ( info
          ( ( Register
                <$> switch (long "silent" <> help "Cancel informational output")
                <*> switch (long "json" <> help "Display information as json")
            )
              <**> helper
          )
          ( progDesc "Register new events"
              <> header "korr event register -- register events"
          )
      )
      <> command
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
                          <*> argument str (metavar "CALENDAR" <> help "The calendar to create the event in")
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

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with event entries in Korrvigs"
      <> header "korr event -- interface for events"

run :: Cmd -> KorrM ()
run (Register silent json) = do
  evs <- allEvents
  forM_ evs $ \(cal, ics) -> do
    (i, ical, ievent, nw) <- register (cal, ics)
    when nw $ do
      syncOneEvent i cal ics ical ievent >>= processRelData i
      unless silent $
        liftIO $
          if json
            then putStrLn $ "\"" <> unId i <> "\""
            else putStrLn $ "Registered " <> cal <> "/" <> ics <> " as " <> unId i
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
