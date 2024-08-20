module Korrvigs.Cli.Query where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import qualified Korrvigs.Format as Fmt
import Korrvigs.Monad
import Opaleye hiding (optional)
import Options.Applicative
import System.Exit

data Cmd = Cmd
  { _query :: Text,
    _format :: Maybe Text
  }

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  Cmd
    <$> argument str (metavar "QUERY")
    <*> optional (strOption (long "format"))

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Queries entries by some criterion"
      <> header "korr query -- Query entries"

run :: Cmd -> KorrM ()
run cmd = do
  fmt <- case Fmt.parse Fmt.entrySpec (fromMaybe defaultFormat $ cmd ^. format) of
    Right fmt -> pure fmt
    Left err -> liftIO $ do
      putStrLn $ "Failed to parse format: " <> T.unpack err
      exitFailure
  ids <- rSelectOne $ do
    entry <- selectTable entriesTable
    pure $ entry ^. sqlEntryName
  forM_ ids $ \i ->
    load i >>= \case
      Nothing -> liftIO $ TIO.putStrLn $ "Failed to load " <> unId i
      Just entry -> case Fmt.run fmt entry of
        Just render -> liftIO $ TIO.putStrLn render
        Nothing -> pure ()
  where
    defaultFormat = "[{kind}] {name}: {title::}"
