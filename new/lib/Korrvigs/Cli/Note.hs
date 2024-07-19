module Korrvigs.Cli.Note where

import Control.Lens hiding (argument)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Korrvigs.Cli.Monad
import Korrvigs.Note (readNote, writeNote)
import Options.Applicative
import System.IO hiding (putStrLn)
import Prelude hiding (putStrLn, readFile, writeFile)

data Cmd = Format {_formatFile :: FilePath, _inplace :: Bool}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "format"
      ( info
          ( ( Format
                <$> argument str (metavar "FILE")
                <*> switch (long "inplace" <> short 'i' <> help "Format FILE in place")
            )
              <**> helper
          )
          ( progDesc "Format a markdown note file"
              <> header "korr note format -- format note"
          )
      )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with note entries in Korrvigs"
      <> header "korr note -- interface for notes"

run :: Cmd -> KorrM ()
run (Format path inline) = do
  doRead <- readNote path
  case doRead of
    Left err -> liftIO . putStrLn $ "Could not parse " <> T.pack path <> ": " <> err
    Right doc -> do
      handle <- if inline then liftIO $ openFile path WriteMode else pure stdout
      doWrite <- writeNote handle doc
      case doWrite of
        Just err -> liftIO . putStrLn $ "Could not write document: " <> err
        Nothing -> pure ()
