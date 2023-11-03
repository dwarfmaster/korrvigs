{-# LANGUAGE LambdaCase #-}

module Korrvigs.Web.Entry.Notes (noteWidget) where

import Control.Monad ((>=>))
import Data.Default (def)
import qualified Data.Text.Lazy.Builder as Bld
import Data.UUID (toString)
import Korrvigs.Definition (Entry (..))
import Korrvigs.Web.Backend
import System.FilePath ((</>))
import Text.Pandoc (PandocMonad, readFileStrict, runIO)
import Text.Pandoc.Error (renderError)
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Highlighting (espresso, styleToCss)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.UTF8 (toText)
import Text.Pandoc.Writers.HTML (writeHtml5)
import Yesod (CssBuilder (..), Html, liftIO, toHtml, toWidget)

readNotePandoc :: PandocMonad m => FilePath -> m Html
readNotePandoc =
  readFileStrict >=> pure . toText >=> readMarkdown rOpts >=> writeHtml5 wOpts
  where
    rOpts :: ReaderOptions
    rOpts = def {readerExtensions = pandocExtensions}
    wOpts :: WriterOptions
    wOpts =
      def
        { writerExtensions = pandocExtensions,
          writerSectionDivs = True
        }

readNote :: FilePath -> Handler Html
readNote path =
  (liftIO $ runIO $ readNotePandoc path) >>= \case
    Left err -> pure $ toHtml $ "Error: " <> renderError err
    Right html -> pure html

noteWidget :: Entry -> Handler Widget
noteWidget entry = do
  root <- korrRoot
  html <- readNote $ path root
  pure $ do
    toWidget html
    toWidget $ CssBuilder $ Bld.fromString $ styleToCss espresso
  where
    path root = root </> (toString $ entry_id entry) </> (entry_notes entry)
