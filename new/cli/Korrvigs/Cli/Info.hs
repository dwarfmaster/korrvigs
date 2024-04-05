module Korrvigs.Cli.Info where

import Control.Lens hiding (argument)
import Control.Monad.Except
import Data.Text.IO (putStrLn)
import Data.Time.Format.ISO8601 (iso8601Show)
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Monad
import Options.Applicative
import Text.Builder (string, text)
import qualified Text.Builder as Bld
import Prelude hiding (putStrLn)

data Cmd = Cmd
  { _json :: Bool,
    _target :: Id
  }

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  Cmd
    <$> switch (long "json" <> help "Display info as json")
    <*> (MkId <$> argument str (metavar "ID"))

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Display information about a Korrvigs entry"
      <> header "korr info -- Show information about entry"

run :: Cmd -> KorrM ()
run (Cmd js i) =
  load i >>= \case
    Nothing -> throwError $ KIdNotFound i
    Just entry ->
      liftIO $ if js then displayEntryJSON entry else displayEntry entry

displayEntry :: Entry -> IO ()
displayEntry entry =
  putStrLn $
    Bld.run $
      "Name: "
        <> (text . unId $ entry ^. name)
        <> maybe "" (\dt -> "\nDate: " <> string (iso8601Show dt)) (entry ^. date)
        <> maybe "" (\dur -> "\nDuration: " <> string (iso8601Show dur)) (entry ^. duration)
        <> "\nUnimplemented"

displayEntryJSON :: Entry -> IO ()
displayEntryJSON entry = putStrLn "Unimplemented"
