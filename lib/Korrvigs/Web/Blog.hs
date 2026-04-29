module Korrvigs.Web.Blog where

import Conduit (throwM)
import Control.Lens
import Data.Binary.Builder
import Data.Default
import Data.Foldable (fold)
import qualified Data.Foldable1 as F1
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.XML.Types as XML
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Blog
import Korrvigs.Metadata.Blog.Export (BlogPageContent (..), renderPageContent)
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
import qualified Text.Atom.Feed as A
import Text.Atom.Feed.Export (xmlFeed)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.XML (fromXMLDocument, renderLBS)
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
  page <- generateArchivePage "Archive" (alltags preppedTags) =<< loadForTag Nothing Nothing
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
  page <- generateArchivePage ("Archive for " <> tag) mempty =<< loadForTag (Just tag) Nothing
  pure $ toTypedContent page

getBlogAtomAllR :: Handler TypedContent
getBlogAtomAllR = renderAtom Nothing "Blog feed" =<< loadForTag Nothing (Just 10)

getBlogAtomTagR :: Text -> Handler TypedContent
getBlogAtomTagR tag =
  renderAtom (Just tag) ("Blog feed for tag " <> tag) =<< loadForTag (Just tag) (Just 10)

generateArchivePage :: Text -> Html -> [(Text, Day, Text)] -> Handler Html
generateArchivePage title extra entries = do
  cfg <- blogConfig
  mtdt <- loadMtdt cfg
  preppedEntries <- mapM (\(u, d, t) -> (,d,t) <$> renderUrl (BlogPostNote u)) entries
  let byYear = NE.groupBy (\(_, d1, _) (_, d2, _) -> getYear d1 == getYear d2) preppedEntries
  let content =
        mconcat
          [ H.h1 $ H.toMarkup title,
            mconcat $ renderYear <$> byYear,
            extra
          ]
  renderPageContent $ BlogPageContent content mtdt title renderUrl
  where
    getYear :: Day -> Year
    getYear = dayPeriod
    renderYear :: NE.NonEmpty (Text, Day, Text) -> Html
    renderYear yearEntries =
      mconcat
        [ H.h2 $ H.toMarkup $ show $ getYear $ view _2 $ F1.head $ yearEntries,
          H.ul $ fold $ H.li . renderEntry <$> yearEntries
        ]
    renderEntry (u, d, t) =
      mconcat
        [ H.span (H.toMarkup $ formatTime defaultTimeLocale "%F" d) ! A.class_ "postdate",
          " ",
          H.a (H.toMarkup t) ! A.href (H.toValue u)
        ]

renderAtom :: Maybe Text -> Text -> [(Text, Day, Text)] -> Handler TypedContent
renderAtom tag title entries = do
  xml <- xmlFeed <$> generateAtomFor tag title entries
  let doc =
        fromXMLDocument $
          XML.Document
            { XML.documentPrologue =
                XML.Prologue
                  { XML.prologueDoctype = Nothing,
                    XML.prologueBefore = [],
                    XML.prologueAfter = []
                  },
              XML.documentRoot = xml,
              XML.documentEpilogue = []
            }
  case doc of
    Left _ -> throwM $ KMiscError "Failed to convert from atom xml"
    Right d -> do
      let lbs = renderLBS def d
      pure $ toTypedContent (typeAtom, ContentBuilder (fromLazyByteString lbs) Nothing)

generateAtomFor :: Maybe Text -> Text -> [(Text, Day, Text)] -> Handler A.Feed
generateAtomFor tag title entries = do
  uri <- renderUrl $ maybe BlogAtom BlogAtomTag tag
  atomEntries <- mapM generateAtomEntryFor entries
  let lastDay = fromMaybe (fromGregorian 1970 01 01) $ view _2 <$> listToMaybe entries
  let date = T.pack $ iso8601Show $ UTCTime lastDay 0
  pure $
    (A.nullFeed uri (A.TextString title) date)
      { A.feedGenerator =
          Just $
            A.Generator
              { A.genURI = Just "https://github.com/dwarfmaster/korrvigs",
                A.genVersion = Nothing,
                A.genText = "korrvigs"
              },
        A.feedEntries = atomEntries
      }

generateAtomEntryFor :: (Text, Day, Text) -> Handler A.Entry
generateAtomEntryFor (nm, day, title) = do
  uri <- renderUrl $ BlogPostNote nm
  let date = T.pack $ iso8601Show $ UTCTime day 0
  pure $
    (A.nullEntry uri (A.TextString title) date)
      { A.entryPublished = Just date
      }
