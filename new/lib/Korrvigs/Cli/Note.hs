module Korrvigs.Cli.Note where

import Control.Lens hiding (argument)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Text.IO (putStrLn, readFile, writeFile)
import Korrvigs.Cli.Monad
import Korrvigs.Note.Pandoc
import Options.Applicative
import Text.Pandoc hiding (Format)
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

readerOptions :: ReaderOptions
readerOptions =
  def
    { readerExtensions = pandocExtensions
    }

writerOptions :: WriterOptions
writerOptions =
  def
    { writerExtensions = pandocExtensions
    }

run :: Cmd -> KorrM ()
run (Format path inline) = do
  file <- liftIO $ readFile path
  doRead <- liftIO . runIO $ readMarkdown readerOptions file
  case doRead of
    Left err -> liftIO $ putStrLn $ "Could not parse " <> T.pack path <> ": " <> renderError err
    Right pandoc -> do
      let rpandoc = toPandoc $ parsePandoc pandoc
      doWrite <- liftIO . runIO $ writeMarkdown writerOptions rpandoc
      case doWrite of
        Left err -> liftIO $ putStrLn $ "Could not generate markdown: " <> renderError err
        Right md | inline -> liftIO $ writeFile path md
        Right md -> liftIO $ putStrLn md
