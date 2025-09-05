module Korrvigs.Cli.Link where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Calendar.JSON
import Korrvigs.Calendar.Sync
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New
import Korrvigs.Entry
import Korrvigs.Event.ICalendar
import Korrvigs.Event.Sync
import Korrvigs.File.Sync
import Korrvigs.Link.JSON
import Korrvigs.Link.New
import Korrvigs.Link.Sync (allJSONs)
import Options.Applicative
import Prelude hiding (readFile, writeFile)

data Cmd
  = New {_newUrl :: Text, _newOptions :: NewLink}
  | Update

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "new"
      ( info
          ( ( New
                <$> argument str (metavar "URL")
                <*> ( NewLink
                        <$> newEntryOptions
                        <*> switch (long "offline" <> help "Do not download information about URL")
                    )
            )
              <**> helper
          )
          ( progDesc "Add a new URL if it is not already present, and print its ID"
              <> header "korr link new -- insert new link"
          )
      )
      <> command "update" (info (pure Update) mempty)

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with link entries in Korrvigs"
      <> header "korr link -- interface for links"

run :: Cmd -> KorrM ()
run (New url options) = do
  i <- new url options
  liftIO $ putStrLn $ T.unpack $ unId i
run Update = do
  liftIO $ putStrLn "> Updating links"
  do
    jsons <- allJSONs
    forM_ jsons $ \json -> liftIO $ do
      content <- readFile json
      case eitherDecode content of
        Left err -> putStrLn $ "Failed to load " <> json <> ": " <> show err
        Right ojs -> do
          putStrLn $ "Updating " <> json
          let js =
                ojs
                  & lkjsTitle .~ (ojs ^? lkjsMetadata . at "title" . _Just . _String)
                  & lkjsMetadata %~ M.delete "meta" . M.delete "title"
          writeFile json $ encode js
  liftIO $ putStrLn "> Updating files"
  do
    files <- allFiles
    forM_ files $ \file -> liftIO $ do
      let meta = metaPath file
      content <- readFile meta
      case eitherDecode content of
        Left err -> putStrLn $ "Failed to load " <> meta <> ": " <> show err
        Right omtdt -> do
          putStrLn $ "Updating " <> meta
          let mtdt =
                omtdt
                  & exTitle .~ (omtdt ^? annoted . at "title" . _Just . _String)
                  & annoted %~ M.delete "title"
          writeFile meta $ encode mtdt
  liftIO $ putStrLn "> Updating calendars"
  do
    cals <- allCalendars
    forM_ cals $ \cal -> liftIO $ do
      content <- readFile cal
      case eitherDecode content of
        Left err -> putStrLn $ "Failed to load " <> cal <> ": " <> show err
        Right ocdat -> do
          putStrLn $ "Updating " <> cal
          let cdat =
                ocdat
                  & cljsTitle .~ (ocdat ^? cljsMetadata . at "title" . _Just . _String)
                  & cljsMetadata %~ M.delete "title"
          writeFile cal $ encode cdat
  liftIO $ putStrLn "> Updating events"
  do
    evs <- allEvents
    forM_ evs $ \ev -> liftIO $ do
      ical <- parseICalFile ev
      case ical of
        Left err -> putStrLn $ "Failed to load " <> ev <> ": " <> show err
        Right ic -> do
          putStrLn $ "Updating " <> ev
          let nical = ic & icEvent . _Just . iceMtdt %~ M.delete "title"
          writeFile ev $ renderICalFile nical
