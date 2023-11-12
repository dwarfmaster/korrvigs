module Korrvigs.Tree where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (def)
import Data.Text (Text, unpack)
import Data.Text.IO (writeFile)
import Data.UUID (UUID)
import qualified Data.UUID as U
import Korrvigs.Definition
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Text.Pandoc (PandocMonad, runIO)
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Prelude hiding (writeFile)

-- Returns Right True if the directory already existed. On failure, if it
-- didn't exists, it is removed.
createEntry :: MonadIO m => FilePath -> UUID -> Text -> m Bool
createEntry root uuid md = liftIO $ do
  dirExists <- Dir.doesDirectoryExist dir
  Dir.createDirectoryIfMissing True dir
  exists <- Dir.doesFileExist mdPath
  if exists
    then pure ()
    else writeFile mdPath ""
  pure dirExists
  where
    dir :: FilePath
    dir = root </> U.toString uuid
    mdPath :: FilePath
    mdPath = dir </> unpack md

-- Write the markdown to the file of an entry, formatting it first with pandoc
writeNotes :: MonadIO m => FilePath -> Entry -> Text -> m ()
writeNotes root entry md = do
  formatted <- do
    doFormat <- liftIO $ runIO $ format md
    pure $ case doFormat of
      Left _ -> md -- On error, we write the unformatted markdown
      Right nmd -> nmd
  let path = entryMdPath root entry
  liftIO $ writeFile path formatted
  where
    format :: PandocMonad m => Text -> m Text
    format = readMarkdown rOpts >=> writeMarkdown wOpts
    rOpts :: ReaderOptions
    rOpts = def {readerExtensions = pandocExtensions}
    wOpts :: WriterOptions
    wOpts =
      def
        { writerExtensions = pandocExtensions,
          writerSectionDivs = True
        }
