module Korrvigs.Cli.Mtdt where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.Decoding
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format
import Data.Time.Format.ISO8601
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Monad.Metadata
import Options.Applicative
import System.Exit

data Cmd
  = Set {_setEntry :: Id, _setMtdt :: Text, _setValue :: Text}
  | Rm {_rmEntry :: Id, _rmMtdt :: Text}
  | SetTitle {_setTitleEntry :: Id, _setTitleValue :: Text}
  | SetDate {_setDateEntry :: Id, _setDateValue :: Text, _setDateDuration :: Maybe Text}

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "set"
      ( info
          ( Set . MkId
              <$> (argument str $ metavar "ID" <> help "Entry to edit")
              <*> (argument str $ metavar "MTDT" <> help "Metadata to set")
              <*> (argument str $ metavar "VALUE" <> help "Value to set, must be valid JSON") <**> helper
          )
          ( progDesc "Set metadata to specified value"
              <> header "korr mtdt set -- set metadata"
          )
      )
      <> command
        "rm"
        ( info
            ( Rm . MkId
                <$> (argument str $ metavar "ID" <> help "Entry to edit")
                <*> (argument str $ metavar "MTDT" <> help "Metadata to remove")
                  <**> helper
            )
            ( progDesc "Remove metadata from entry"
                <> header "korr mtdt rm -- remove metadata"
            )
        )
      <> command
        "set-title"
        ( info
            ( SetTitle . MkId
                <$> (argument str $ metavar "ID" <> help "Entry to set title to")
                <*> (argument str $ metavar "TITLE" <> help "New title to set")
                  <**> helper
            )
            ( progDesc "Set entry title"
                <> header "korr mtdt set-title -- set title"
            )
        )
      <> command
        "set-date"
        ( info
            ( SetDate . MkId
                <$> (argument str $ metavar "ID" <> help "Entry to set title to")
                <*> (argument str $ metavar "DATE" <> help "New date to set, in the format iso8601")
                <*> (optional $ option str (long "duration" <> metavar "DUR" <> help "Optional duration to set, in format D-H:M"))
                  <**> helper
            )
            ( progDesc "Set entry date and duration"
                <> header "korr mtdt set-date -- set date"
            )
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Modify entries metadata"
      <> header "korr mtdt -- modify metadata"

run :: Cmd -> KorrM ()
run (Set entry mtdt val) = case eitherDecodeStrictText val of
  Left err -> liftIO $ do
    putStrLn $ "Failed to parse JSON: " <> err
    exitWith $ ExitFailure 1
  Right v -> runOn entry $ \e -> updateMetadata e (M.singleton mtdt v) []
run (Rm entry mtdt) = runOn entry $ \e -> updateMetadata e M.empty [mtdt]
run (SetTitle entry title) = runOn entry $ flip updateTitle $ Just title
run (SetDate entry date dur) = case iso8601ParseM (T.unpack date) of
  Nothing -> liftIO $ do
    putStrLn $ "Failed to parse date"
    exitWith $ ExitFailure 1
  Just dt -> do
    duration <- case dur of
      Nothing -> pure Nothing
      Just d -> case parseTimeM True defaultTimeLocale "%d-%h:%m" (T.unpack d) of
        Nothing -> liftIO $ do
          putStrLn $ "Failed to parse duration"
          exitWith $ ExitFailure 1
        Just duration -> pure $ Just duration
    runOn entry $ \e -> do
      updateDate e $ Just dt
      forM_ duration $ updateDuration e . Just

runOn :: Id -> (Entry -> KorrM ()) -> KorrM ()
runOn i act = do
  entry <- load i >>= throwMaybe (KCantLoad i "Failed to load entry to update metadata")
  act entry
