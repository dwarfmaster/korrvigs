module Korrvigs.Web.Blog where

import Control.Lens
import Data.Binary.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
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
import Korrvigs.Web.Backend
import Korrvigs.Web.Compute
import Korrvigs.Web.Download
import Korrvigs.Web.Routes
import Opaleye
import System.FilePath
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
  BlogConfig <$> lookupHost <*> pure (MkId "Blog") <*> pure False

topEntries :: Id -> Handler (Maybe BlogUrl)
topEntries tgt = do
  cfg <- blogConfig
  topLevelSql <- rSelectMtdt BlogFiles $ sqlId $ cfg ^. blogCfgNote
  case topLevelSql of
    Nothing -> pure Nothing
    Just topFiles -> do
      let topFile = find ((== BlogFromNote tgt) . parseBlogTopContent cfg . snd) $ M.toList topFiles
      pure $ BlogTopLevel . fst <$> topFile

getBlogR :: Handler TypedContent
getBlogR = getBlogTopR "default.html"

getBlogContent :: BlogContent -> Handler TypedContent
getBlogContent (BlogFromFile i) = getEntryDownloadR $ WId i
getBlogContent (BlogFromNote i) = do
  cfg <- blogConfig
  mtdt <- loadMtdt cfg
  menu <- loadMenu cfg
  toTypedContent <$> renderPost menu renderUrl topEntries mtdt i
getBlogContent (BlogFromCode i cd) = do
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
getBlogContent (BlogFromComp i cmp) = getEntryComputeNamedR (WId i) cmp cmp
getBlogContent BlogFromArchive = do
  tags <- loadTags
  cfg <- blogConfig
  mtdt <- loadMtdt cfg
  menu <- loadMenu cfg
  page <- generateArchivePage mtdt (cfg ^. blogCfgOnlyPublished) menu renderUrl Nothing tags
  pure $ toTypedContent page
getBlogContent (BlogFromArchiveTag tag) = do
  cfg <- blogConfig
  mtdt <- loadMtdt cfg
  menu <- loadMenu cfg
  page <- generateArchivePage mtdt (cfg ^. blogCfgOnlyPublished) menu renderUrl (Just tag) []
  pure $ toTypedContent page
getBlogContent BlogFromAtom = do
  cfg <- blogConfig
  atomContent <$> renderAtom (cfg ^. blogCfgOnlyPublished) renderUrl topEntries Nothing "Blog feed"
getBlogContent (BlogFromAtomTag tag) = do
  cfg <- blogConfig
  atomContent <$> renderAtom (cfg ^. blogCfgOnlyPublished) renderUrl topEntries (Just tag) ("Blog feed for " <> tag)

atomContent :: LBS.ByteString -> TypedContent
atomContent lbs =
  toTypedContent (typeAtom, ContentBuilder (fromLazyByteString lbs) Nothing)

getBlogTopR :: Text -> Handler TypedContent
getBlogTopR file = do
  cfg <- blogConfig
  str <- loadStructure cfg
  case M.lookup (BlogTopLevel file) (str ^. blogFiles) of
    Nothing -> notFound
    Just content -> getBlogContent content

getBlogFileR :: Text -> Handler TypedContent
getBlogFileR filename = do
  file <- rSelectOne $ do
    entry <- selectTable entriesTable
    file <- baseSelectTextMtdt BlogFile $ entry ^. sqlEntryId
    where_ $ file .== sqlStrictText filename
    pure $ entry ^. sqlEntryName
  case file of
    Nothing -> notFound
    Just fileId -> getBlogContent $ BlogFromFile fileId

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
  getBlogContent $ BlogFromNote postId

getBlogPostCompR :: Text -> Text -> Handler TypedContent
getBlogPostCompR postname cmpWithExt = do
  let cmp = T.pack $ dropExtension $ T.unpack cmpWithExt
  postId <- lookupPost postname
  getEntryComputeNamedR (WId postId) cmp cmpWithExt

getBlogArchiveAllR :: Handler TypedContent
getBlogArchiveAllR = getBlogContent BlogFromArchive

getBlogArchiveTagR :: Text -> Handler TypedContent
getBlogArchiveTagR = getBlogContent . BlogFromArchiveTag

getBlogAtomAllR :: Handler TypedContent
getBlogAtomAllR = getBlogContent BlogFromAtom

getBlogAtomTagR :: Text -> Handler TypedContent
getBlogAtomTagR = getBlogContent . BlogFromAtomTag
