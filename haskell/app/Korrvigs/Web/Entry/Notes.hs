module Korrvigs.Web.Entry.Notes (noteWidget) where

import Control.Monad ((>=>))
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Bld
import Data.UUID (toString)
import Korrvigs.Definition (EntityRef (..), Entry (..))
import Korrvigs.Pandoc
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.UUID as U
import System.FilePath ((</>))
import Text.Pandoc (PandocMonad, readFileStrict, runIO)
import Text.Pandoc.Error (renderError)
import Text.Pandoc.Highlighting (espresso, styleToCss)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.UTF8 (toText)
import Text.Pandoc.Writers.HTML (writeHtml5)
import Yesod (CssBuilder (..), Html, getUrlRender, liftIO, toHtml, toWidget)

readNotePandoc :: PandocMonad m => (EntityRef -> m Text) -> FilePath -> m Html
readNotePandoc renderLink =
  readFileStrict
    >=> pure . toText
    >=> readMarkdown readerOptions
    >=> displayLinks renderLink
    >=> writeHtml5 writerOptions

readNote :: FilePath -> Handler Html
readNote path = do
  render <- getUrlRender
  md <- liftIO $ runIO $ readNotePandoc (pure . render . renderLink) path
  case md of
    Left err -> pure $ toHtml $ "Error: " <> renderError err
    Right html -> pure html
  where
    renderLink :: EntityRef -> Route Korrvigs
    renderLink (EntityRef uuid Nothing Nothing) = EntryR (U.UUID uuid)
    renderLink (EntityRef uuid Nothing (Just query)) = EntryQueryR (U.UUID uuid) query
    renderLink (EntityRef uuid (Just sub) Nothing) = EntitySubR (U.UUID uuid) sub
    renderLink (EntityRef uuid (Just sub) (Just query)) =
      EntitySubQueryR (U.UUID uuid) sub query

noteWidget :: Entry -> Handler Widget
noteWidget entry = do
  root <- korrRoot
  html <- readNote $ path root
  pure $ do
    toWidget html
    toWidget $ CssBuilder $ Bld.fromString $ styleToCss espresso
  where
    path root = root </> (toString $ entry_id entry) </> (entry_notes entry)
