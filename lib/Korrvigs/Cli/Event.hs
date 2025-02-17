module Korrvigs.Cli.Event where

import Conduit
import Control.Lens hiding (argument)
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Korrvigs.Actions (processRelData)
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New
import Korrvigs.Entry
import Korrvigs.Event.New
import Korrvigs.Event.Sync
import Korrvigs.Event.VDirSyncer
import qualified Korrvigs.Utils.DAV.Cal as DAV
import Korrvigs.Utils.DateParser (dayParser)
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Options.Applicative
import System.IO hiding (putStrLn, utf8)
import Prelude hiding (putStrLn)

data Cmd
  = Register {_regSilent :: Bool, _regJSON :: Bool}
  | Sync
  | New NewEvent
  | Pull

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
run Pull = do
  liftIO $ putStr "Password: "
  liftIO $ hFlush stdout
  pwd <- liftIO $ T.pack <$> withEcho False getLine
  man <- liftIO $ newManager tlsManagerSettings
  let cdav = DAV.CalDavData "luc" pwd man "https://nextcloud.dwarfmaster.net/remote.php/dav" "dance"
  metags <- DAV.getETags cdav
  forM_ metags $ \etags -> do
    dat <- DAV.getCalData cdav $ M.keys etags
    case dat of
      Left err -> liftIO $ print err
      _ -> pure ()
    forM_ (M.toList etags) $ \(ics, etag) -> do
      liftIO $ print $ ">>> " <> ics <> " -> " <> etag <> " <<<"
      case M.lookup ics <$> dat of
        Right (Just content) -> liftIO $ putStrLn content
        _ -> pure ()

-- Caldav
withEcho :: Bool -> IO a -> IO a
withEcho echo act = do
  old <- hGetEcho stdin
  hSetEcho stdin echo
  r <- act
  hSetEcho stdin old
  pure r
