module Korrvigs.Web.Blog where

import Control.Lens
import Data.Binary.Builder
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Blog
import Korrvigs.Metadata.Blog.Structure
import Korrvigs.Monad
import Korrvigs.Note
import Korrvigs.Note.AST
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import Korrvigs.Web.Compute
import Korrvigs.Web.Download
import Korrvigs.Web.Routes
import Opaleye
import System.FilePath
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Yesod

renderUrlImpl :: BlogUrl -> Route WebData
renderUrlImpl (BlogTopLevel txt) = BlogTopR txt
renderUrlImpl (BlogFilePlain file) = BlogFileR file
renderUrlImpl (BlogPostNote note) = BlogPostR note
renderUrlImpl (BlogComputation note cmp tp) = BlogPostCompR note $ cmp <> "." <> ext
  where
    ext = runTypeExt tp
renderUrlImpl BlogArchive = BlogArchiveAllR
renderUrlImpl (BlogArchiveTag tag) = BlogArchiveTagR tag
renderUrlImpl BlogAtom = BlogAtomAllR
renderUrlImpl (BlogAtomTag tag) = BlogAtomTagR tag

renderUrl :: BlogUrl -> Handler Text
renderUrl u = getUrlRender <*> pure (renderUrlImpl u)

lookupHost :: Handler Text
lookupHost =
  lookupHeader "Host"
    >>= pure . \case
      Nothing -> "localhost"
      Just host ->
        fromMaybe "localhost" $ (T.split (== ':') $ Enc.decodeASCII host) ^? ix 1

blogConfig :: Handler BlogConfig
blogConfig =
  BlogConfig <$> lookupHost <*> pure (MkId "Blog")

getBlogR :: Handler TypedContent
getBlogR = getBlogTopR "default.html"

getBlogTopR :: Text -> Handler TypedContent
getBlogTopR file = do
  cfg <- blogConfig
  str <- loadStructure cfg
  case M.lookup (BlogTopLevel file) (str ^. blogFiles) of
    Nothing -> notFound
    Just (BlogFromFile i) -> getEntryDownloadR $ WId i
    Just (BlogFromNote i) -> toTypedContent <$> renderPost renderUrl (str ^. blogMtdt) i
    Just (BlogFromCode i cd) -> do
      entry <- load i >>= maybe notFound pure
      note <- maybe notFound pure $ entry ^? _Note
      doc <- readNote (note ^. notePath) >>= either (const notFound) pure
      (attr, content) <- maybe notFound pure $ doc ^? docContent . each . bkNamedCode cd
      let classes = attr ^. attrClasses
      let mime = case () of
            _ | "css" `elem` classes -> typeCss
            _ | "html" `elem` classes -> typeHtml
            _ -> typePlain
      pure $ toTypedContent (mime, ContentBuilder (fromByteString $ Enc.encodeUtf8 content) (Just $ T.length content))

getBlogFileR :: Text -> Handler TypedContent
getBlogFileR filename = do
  file <- rSelectOne $ do
    entry <- selectTable entriesTable
    file <- baseSelectTextMtdt BlogFile $ entry ^. sqlEntryId
    where_ $ file .== sqlStrictText filename
    pure $ entry ^. sqlEntryName
  case file of
    Nothing -> notFound
    Just fileId -> getEntryDownloadR $ WId $ MkId fileId

lookupPost :: Text -> Handler Id
lookupPost postname = do
  post <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryKind .== sqlKind Note
    post <- baseSelectTextMtdt BlogPost $ entry ^. sqlEntryId
    where_ $ post .== sqlStrictText postname
    pure $ entry ^. sqlEntryName
  case post of
    Nothing -> notFound
    Just postId -> pure postId

getBlogPostR :: Text -> Handler TypedContent
getBlogPostR postname = do
  postId <- lookupPost postname
  cfg <- blogConfig
  mtdt <- loadMtdt cfg
  toTypedContent <$> renderPost renderUrl mtdt postId

getBlogPostCompR :: Text -> Text -> Handler TypedContent
getBlogPostCompR postname cmpWithExt = do
  let cmp = T.pack $ dropExtension $ T.unpack cmpWithExt
  postId <- lookupPost postname
  getEntryComputeNamedR (WId postId) cmp cmpWithExt

getBlogArchiveAllR :: Handler TypedContent
getBlogArchiveAllR = do
  tags <- rSelect $ distinct $ orderBy (asc id) $ do
    mtdt <- selectTable entriesMetadataTable
    where_ $ mtdt ^. sqlKey .== sqlStrictText (mtdtSqlName BlogTags)
    sqlJsonElementsText $ toNullable $ mtdt ^. sqlValue
  preppedTags <- mapM prepTag tags
  cfg <- blogConfig
  page <- generateArchivePage cfg renderUrl Nothing (alltags preppedTags) =<< loadForTag Nothing Nothing
  pure $ toTypedContent page
  where
    alltags tags =
      mconcat
        [ H.h2 "Tags",
          H.ul $ mconcat tags
        ]
    prepTag t = do
      u <- renderUrl $ BlogArchiveTag t
      pure $ H.li $ H.a (H.toMarkup t) ! A.href (H.toValue u)

getBlogArchiveTagR :: Text -> Handler TypedContent
getBlogArchiveTagR tag = do
  cfg <- blogConfig
  page <- generateArchivePage cfg renderUrl (Just tag) mempty =<< loadForTag (Just tag) Nothing
  pure $ toTypedContent page

getBlogAtomAllR :: Handler TypedContent
getBlogAtomAllR =
  renderAtom renderUrl Nothing "Blog feed" =<< loadForTag Nothing (Just 10)

getBlogAtomTagR :: Text -> Handler TypedContent
getBlogAtomTagR tag =
  renderAtom renderUrl (Just tag) ("Blog feed for tag " <> tag)
    =<< loadForTag (Just tag) (Just 10)
