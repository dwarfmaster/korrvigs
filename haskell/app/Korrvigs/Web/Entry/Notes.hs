module Korrvigs.Web.Entry.Notes (noteWidget, noteEditor) where

import Control.Monad ((>=>))
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Text.Lazy.Builder as Bld
import Data.UUID (UUID, toString)
import Korrvigs.Definition (EntityRef (..), Entry (..), entryMdPath)
import Korrvigs.Entry (lookupEntry)
import Korrvigs.Pandoc
import qualified Korrvigs.Tree as Tree
import Korrvigs.Web.Backend
import Korrvigs.Web.Editor
import qualified Korrvigs.Web.Entry.Pandoc as Pandoc
import Korrvigs.Web.Method
import qualified Korrvigs.Web.UUID as U
import System.FilePath ((</>))
import Text.Pandoc (Pandoc, PandocMonad, readFileStrict, runIO)
import Text.Pandoc.Error (renderError)
import Text.Pandoc.Highlighting (espresso, styleToCss)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.UTF8 (toText)
import Text.Pandoc.Writers.HTML (writeHtml5)
import Yesod
import Prelude hiding (readFile)

readNotePandoc :: PandocMonad m => (EntityRef -> m Text) -> FilePath -> m Pandoc
readNotePandoc renderLink =
  readFileStrict
    >=> pure . toText
    >=> readMarkdown readerOptions
    >=> displayLinks renderLink

readNote :: FilePath -> Handler Html
readNote path = do
  render <- getUrlRender
  md <-
    liftIO $
      runIO $
        readNotePandoc (pure . render . renderLinkPandoc) path
          >>= writeHtml5 writerOptions
  case md of
    Left err -> pure $ toHtml $ "Error: " <> renderError err
    Right html -> pure html

renderLinkPandoc :: EntityRef -> Route Korrvigs
renderLinkPandoc (EntityRef uuid Nothing Nothing) = EntryR (U.UUID uuid)
renderLinkPandoc (EntityRef uuid Nothing (Just query)) = EntryQueryR (U.UUID uuid) query
renderLinkPandoc (EntityRef uuid (Just sub) Nothing) = EntrySubR (U.UUID uuid) sub
renderLinkPandoc (EntityRef uuid (Just sub) (Just query)) =
  EntrySubQueryR (U.UUID uuid) sub query

noteWidget :: Entry -> Handler Widget
noteWidget entry = do
  root <- korrRoot
  render <- getUrlRender
  md <- liftIO $ runIO $ readNotePandoc (pure . render . renderLinkPandoc) $ path root
  widget <- case md of
    Left err -> pure $ toWidget $ "Error: " <> renderError err
    Right pd -> Pandoc.renderPandoc pd
  pure $ do
    [whamlet|<a href=@{EntryEditR (U.UUID (entry_id entry))}>Edit|]
    widget
    toWidget $ CssBuilder $ Bld.fromString $ styleToCss espresso
  where
    path root = root </> toString (entry_id entry) </> entry_notes entry

noteEditor :: Method -> UUID -> (Widget -> Handler Html) -> Handler TypedContent
noteEditor meth uuid render = do
  conn <- pgsql
  mentry <- lookupEntry conn uuid
  case mentry of
    Nothing -> notFound
    Just entry -> do
      root <- korrRoot
      editorHandler meth (cfg root entry) $ render . (page $ entry_name entry)
  where
    page :: Text -> Widget -> Widget
    page title editor =
      [whamlet|
        <h1>
          Edit 
          <a href=@{EntryR (U.UUID uuid)}>
            #{title} 
          notes
        ^{editor}
      |]
    cfg :: FilePath -> Entry -> EditorConfig
    cfg root entry =
      EditorConfig
        { editorContent = liftIO $ readFile $ entryMdPath root entry,
          editorLanguage = Markdown,
          editorSave = \content -> do
            Tree.writeNotes root entry content
            pure $ toTypedContent Null
        }
