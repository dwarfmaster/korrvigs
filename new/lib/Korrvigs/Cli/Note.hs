module Korrvigs.Cli.Note where

import Control.Lens hiding (argument)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Note (readNote, writeNote)
import Korrvigs.Note.New
import Options.Applicative
import System.IO hiding (putStrLn)
import Prelude hiding (putStrLn, readFile, writeFile)

data Cmd
  = Format {_formatFile :: FilePath, _inplace :: Bool}
  | New {_newNote :: NewNote}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser
    ( command
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
        <> command
          "new"
          ( info
              ( ( New
                    <$> ( NewNote
                            <$> argument str (metavar "TITLE")
                            <*> pure Nothing
                        )
                )
                  <**> helper
              )
              ( progDesc "Create a new markdown note file"
                  <> header "korr note new -- Create note"
              )
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
      liftIO $ hClose handle
      case doWrite of
        Just err -> liftIO . putStrLn $ "Could not write document: " <> err
        Nothing -> pure ()
run (New note) = do
  i <- new note
  liftIO $ putStrLn $ "Added note as " <> unId i
