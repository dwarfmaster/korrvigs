module Korrvigs.Cli.Log where

import Control.Lens hiding (argument, ignored)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Log
import Korrvigs.Monad
import Korrvigs.Utils.DateParser
import Opaleye hiding (optional)
import Options.Applicative

data Cmd = Show {_showDay :: Maybe Day, _showJson :: Bool}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "show"
      ( info
          ( ( Show
                <$> optional (argument dayParser $ metavar "DAY" <> help "Day to show the log events for, defaults to the current day")
                <*> switch (long "json" <> help "Export events as json")
            )
              <**> helper
          )
          ( progDesc "Show log events"
              <> header "korr log show -- show log events"
          )
      )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with korrvigs log events"
      <> header "korr log -- Log events utilities"

run :: Cmd -> KorrM ()
run (Show mday json) = do
  day <- case mday of
    Just day -> pure day
    Nothing -> liftIO $ utctDay <$> getCurrentTime
  sqlEvents <- rSelect $ do
    ev <- selectTable logEventTable
    where_ $ isNull $ ev ^. evsqlParent
    let start = UTCTime day 0
    where_ $ ev ^. evsqlTime .>= sqlUTCTime start
    where_ $ ev ^. evsqlTime .<= sqlUTCTime (addUTCTime nominalDay start)
    pure ev
  events <- withSQL $ \conn -> liftIO $ loadEvents conn sqlEvents
  if json
    then forM_ events $ liftIO . LTIO.putStr . encodeToLazyText
    else forM_ events $ liftIO . showEvent 0

showEvent :: Int -> LogEvent -> IO ()
showEvent indent ev = do
  TIO.putStr tabs
  case ev ^. logLevel of
    LogTrace -> TIO.putStr " . "
    LogInfo -> TIO.putStr "(i)"
    LogWarning -> TIO.putStr "(W)"
    LogError -> TIO.putStr "/!\\"
  TIO.putStr " "
  putStr $ iso8601Show $ ev ^. logTime
  TIO.putStr $ " [" <> ev ^. logModule <> ":" <> T.pack (show $ ev ^. logLine) <> "] "
  TIO.putStr $ case ev ^. logData of
    MiscEvent txt -> prepText txt
    LoadEvent i -> "Can't load: " <> unId i
    ParseErrorEvent pars src err -> "Can't parse (" <> pars <> "): \"" <> prepText err <> "\" <- \"" <> prepText src <> "\""
    NewEntryEvent kd i -> "Created entry (" <> displayKind kd <> "): " <> unId i
    EntryAlreadyExistsEvent kd i -> "Found entry (" <> displayKind kd <> "): " <> unId i
    MissingCredentialEvent cred -> "Missing credential: " <> cred
  putStr "\n"
  forM_ (ev ^. logChilds) $ showEvent $ indent + 1
  where
    tabs = mconcat (replicate (indent - 1) (" |  " :: Text)) <> " +> "
    off = mconcat (replicate indent (" |  " :: Text)) <> "    "
    prepText = T.dropWhileEnd (== '\n') . T.unlines . prepLines . T.lines
    prepLines [] = []
    prepLines (ln : lns) = ln : fmap (off <>) lns
