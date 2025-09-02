module Korrvigs.Cli.Import where

import Control.Lens hiding (argument)
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New
import Korrvigs.Entry
import Korrvigs.Entry.New
import qualified Korrvigs.Metadata.Media.New as Media
import Korrvigs.Metadata.Media.Ontology
import Options.Applicative
import System.IO

data Cmd = Cmd
  { _cmdInput :: Text,
    _cmdNew :: NewEntry,
    _cmdFile :: Bool,
    _cmdType :: Maybe MediaType,
    _cmdCapture :: Bool
  }

makeLenses ''Cmd

bool :: ReadM Bool
bool = maybeReader $ \case
  "true" -> Just True
  "false" -> Just False
  _ -> Nothing

mediaType :: ReadM MediaType
mediaType = maybeReader $ flip M.lookup mediaTypeMap . T.pack

parser' :: Parser Cmd
parser' =
  Cmd
    <$> argument str (metavar "INPUT" <> help "Text to import from, either an URL, Bibtex/RIS content, or '-' to read from stdin")
    <*> newEntryOptions
    <*> switch (long "file" <> short 'f' <> help "Interpret INPUT as the path to a file containing the input")
    <*> optional (option mediaType $ long "type" <> short 't' <> help "The type of the media to import")
    <*> (fromMaybe True <$> optional (option bool $ long "capture" <> help "Should the media be captured, default to true"))

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Import media"
      <> header "korr import -- media importing"

run :: Cmd -> KorrM ()
run cmd = do
  cInput <-
    if cmd ^. cmdInput == "-"
      then liftIO $ TIO.hGetContents stdin
      else pure $ cmd ^. cmdInput
  input <-
    if cmd ^. cmdFile
      then liftIO $ TIO.readFile $ T.unpack cInput
      else pure cInput
  let new =
        Media.NewMedia
          { Media._nmEntry = cmd ^. cmdNew,
            Media._nmInput = input,
            Media._nmType = cmd ^. cmdType,
            Media._nmCapture = cmd ^. cmdCapture
          }
  i <- Media.new new
  liftIO $ putStrLn $ T.unpack $ unId i
