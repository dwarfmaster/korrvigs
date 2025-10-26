module Korrvigs.Note.Download (downloadInformation) where

import Conduit (liftIO)
import Control.Applicative
import Control.Exception hiding (try)
import Control.Lens hiding (noneOf)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types hiding (parse)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Foldable
import Data.ISBN
import Data.List
import Data.List.Split (divvy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import qualified Data.Vector as V
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import qualified Korrvigs.Note.Download.Books as Books
import qualified Korrvigs.Note.Download.Video as Vid
import Korrvigs.Utils (rightToMaybe)
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Pandoc (pdExtractMtdt)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.Mime
import Network.URI
import Text.HTML.TagSoup
import Text.Pandoc hiding (Link, getCurrentTimeZone)
import Text.Parsec hiding ((<|>))

makeURIAbsolute :: Text -> Text -> Text
makeURIAbsolute url ressource = fromMaybe ressource $ do
  ref <- parseURIReference $ T.unpack ressource
  uri <- parseURI $ T.unpack url
  pure $ T.pack $ show $ ref `relativeTo` uri

contentTypeP :: (Stream s Identity Char) => Parsec s u (MimeType, BS.ByteString -> Maybe Text)
contentTypeP = do
  mime <- many1 $ noneOf ";"
  let decUtf8 = rightToMaybe . Enc.decodeUtf8'
  charset <- option decUtf8 $ try $ do
    void $ char ';'
    spaces
    void $ string "charset="
    charset <- many1 $ noneOf ";"
    pure $ case CI.foldedCase (CI.mk charset) of
      "ascii" -> Enc.decodeASCII'
      "iso-8859-1" -> Just . Enc.decodeLatin1
      _ -> decUtf8
  pure (Enc.encodeUtf8 $ T.pack mime, charset)

isHttpException :: SomeException -> Bool
isHttpException e = isJust (fromException e :: Maybe HttpException)

extractImage :: Text -> Text -> Endo NewEntry
extractImage website url =
  Endo $ neCover %~ maybe (Just $ makeURIAbsolute website url) Just

-- Map property name to metadata name and extractor
htmlMetas :: Text -> Map Text (Text -> Endo NewEntry)
htmlMetas url =
  M.fromList
    [ -- Standard metadata
      ("author", extractList Authors),
      ("description", extractText Abstract),
      -- OpenGraph
      ("og:title", extractTitle),
      ("og:description", extractText Abstract),
      ("og:locale", extractLanguage),
      ("og:image", extractImage url),
      -- Twitter
      ("twitter:title", extractTitle),
      ("twitter:description", extractText Abstract),
      -- Article
      ("article:published_time", extractISO8601)
    ]
  where
    extractTitle t = Endo $ neTitle %~ maybe (Just t) Just
    extractText :: (ExtraMetadata mtdt, MtdtType mtdt ~ Text) => mtdt -> Text -> Endo NewEntry
    extractText mtdt v = Endo $ setMtdtValueLazy mtdt v
    extractList mtdt v = Endo $ setMtdtValueLazyV mtdt (Array $ V.singleton $ String v)
    extractLanguage :: Text -> Endo NewEntry
    extractLanguage loc
      | "en_" `T.isPrefixOf` loc = Endo $ setMtdtValueLazy Language "en"
      | "fr_" `T.isPrefixOf` loc = Endo $ setMtdtValueLazy Language "fr"
      | otherwise = mempty
    extractISO8601 :: Text -> Endo NewEntry
    extractISO8601 dt = case iso8601ParseM $ T.unpack dt of
      Nothing -> mempty
      Just zt -> Endo $ neDate ?~ localDay (zonedTimeToLocalTime zt)

data LDJsonData = LDJsonData
  { _ldContext :: Text,
    _ldName :: Maybe Text,
    _ldAuthor :: Maybe [Text],
    _ldPublisher :: Maybe Text,
    _ldAbout :: Maybe Text,
    _ldImage :: Maybe Text,
    _ldPages :: Maybe Int,
    _ldLanguage :: Maybe Text,
    _ldISBN :: Maybe Text
  }
  deriving (Show)

makeLenses ''LDJsonData

parseAuthors :: Maybe Value -> Parser (Maybe [Text])
parseAuthors Nothing = pure Nothing
parseAuthors (Just (String author)) = pure $ Just [author]
parseAuthors (Just (Array authors)) = do
  parsed <- mapM (withObject "LD Authors" $ \obj -> obj .: "name") authors
  pure $ Just $ toList parsed
parseAuthors _ = fail "Expected string or array for LD authors"

instance FromJSON LDJsonData where
  parseJSON = withObject "LDJsonData" $ \obj ->
    LDJsonData
      <$> obj .: "@context"
      <*> obj .:? "name"
      <*> (parseAuthors =<< obj .:? "author")
      <*> obj .:? "publisher"
      <*> obj .:? "about"
      <*> obj .:? "image"
      <*> obj .:? "numberOfPages"
      <*> obj .:? "inLanguage"
      <*> obj .:? "isbn"

isLDSchema :: Text -> Bool
isLDSchema "http://www.schema.org" = True
isLDSchema "http://schema.org" = True
isLDSchema "https://www.schema.org" = True
isLDSchema "https://schema.org" = True
isLDSchema _ = False

ldToMeta :: LDJsonData -> Endo NewEntry
ldToMeta ld
  | isLDSchema (ld ^. ldContext) =
      mconcat $
        Endo
          <$> [ maybe id (\t -> neTitle %~ maybe (Just t) Just) (ld ^. ldName),
                setMtdtValueLazyM Authors (ld ^. ldAuthor),
                setMtdtValueLazyM Publisher (singleton <$> ld ^. ldPublisher),
                setMtdtValueLazyM Abstract (ld ^. ldAbout),
                maybe id (\c -> neCover %~ maybe (Just c) Just) (ld ^. ldImage),
                setMtdtValueLazyM Pages (ld ^. ldPages),
                setMtdtValueLazyM Language (ld ^. ldLanguage >>= parseLanguage),
                setMtdtValueLazyM ISBNMtdt (ld ^. ldISBN >>= parseISBN)
              ]
  where
    parseISBN :: Text -> Maybe [ISBN]
    parseISBN isbn = case validateISBN isbn of
      Left _ -> Nothing
      Right i -> Just [i]
    parseLanguage :: Text -> Maybe Text
    parseLanguage "English" = Just "en"
    parseLanguage "French" = Just "fr"
    parseLanguage _ = Nothing
ldToMeta _ = mempty

extractHTMLMeta :: (MonadKorrvigs m) => Text -> Text -> m (Endo NewEntry)
extractHTMLMeta url txt =
  fmap mconcat . sequence $
    [ Vid.nebula url tags,
      Books.manytoon url tags,
      Books.goodreads url tags,
      Books.bedetheque url tags,
      Books.webtoons url tags,
      pure . mconcat $ matchLD <$> divvy 2 1 html,
      pure . mconcat $ matchTitle <$> divvy 2 1 html,
      pure . mconcat $ matchRSS <$> html,
      pure . mconcat $ matchMeta <$> html,
      pure matchLanguage
    ]
  where
    tags = parseTags txt
    html = takeWhile (/= TagClose ("head" :: Text)) tags
    matchRSS :: Tag Text -> Endo NewEntry
    matchRSS (TagOpen "link" attrs)
      | any (\(k, v) -> k == "rel" && v == "alternate") attrs = fromMaybe mempty $ do
          tp <- find ((== "type") . fst) attrs
          guard $ snd tp == "application/rss+xml" || snd tp == "application/atom+xml"
          ref <- snd <$> find ((== "href") . fst) attrs
          guard $ T.length ref > 0
          pure $ Endo $ setMtdtValueLazy Feed $ makeURIAbsolute url ref
    matchRSS _ = mempty
    matchMeta :: Tag Text -> Endo NewEntry
    matchMeta (TagOpen "meta" attrs) = fromMaybe mempty $ do
      nm <- lookup "name" attrs <|> lookup "property" attrs
      content <- lookup "content" attrs
      extractor <- M.lookup nm $ htmlMetas url
      pure $ extractor content
    matchMeta _ = mempty
    matchTitle :: [Tag Text] -> Endo NewEntry
    matchTitle [TagOpen "title" _, TagText title] =
      Endo $ neTitle %~ maybe (Just title) Just
    matchTitle _ = mempty
    matchLD :: [Tag Text] -> Endo NewEntry
    matchLD [tagOpen, TagText json]
      | tagOpen ~== TagOpen ("script" :: Text) [("type", "application/ld+json")] =
          case eitherDecode (LEnc.encodeUtf8 $ LT.fromStrict json) of
            Left _ -> mempty
            Right ld -> ldToMeta ld
    matchLD _ = mempty
    matchLanguage :: Endo NewEntry
    matchLanguage =
      if "fr" `T.isSuffixOf` url
        then Endo $ setMtdtValueLazy Language "fr"
        else mempty

processPandocMtdt :: Map Text Value -> Endo NewEntry
processPandocMtdt = mconcat . mapMaybe process . M.toList
  where
    process :: (Text, Value) -> Maybe (Endo NewEntry)
    process (key, val) = (\k -> Endo $ neMtdt . at (CI.mk k) %~ maybe (Just val) Just) <$> M.lookup key pdMtdt
    pdMtdt :: Map Text Text
    pdMtdt =
      M.fromList
        [ mkMtdt Authors "authors",
          mkMtdt Abstract "abstract"
        ]
    mkMtdt :: (ExtraMetadata mtdt) => mtdt -> Text -> (Text, Text)
    mkMtdt mtdt pd = (pd, mtdtSqlName mtdt)

extractPandocMtdt :: Pandoc -> Endo NewEntry
extractPandocMtdt pd = Endo (setContent . setTitle) <> processPandocMtdt mtdt
  where
    (content, mtdt) = pdExtractMtdt pd
    setContent = if T.null content then id else neContent ?~ content
    setTitle = maybe id (neTitle ?~) $ M.lookup "title" mtdt >>= fromJSONM

downloadInformation :: (MonadKorrvigs m) => Text -> m (Endo NewEntry)
downloadInformation uri = do
  req <- parseRequest $ T.unpack uri
  resp <- liftIO $ tryJust (guard . isHttpException) $ httpBS req
  case resp of
    Left _ -> pure mempty
    Right response -> do
      let status = getResponseStatus response
      if statusCode status /= 200
        then pure mempty
        else do
          let ct = getResponseHeader "Content-Type" response
          let r = parse contentTypeP "" <$> ct
          case r of
            (Right ("text/html", decoder)) : _ -> case decoder (getResponseBody response) of
              Just content -> do
                htmlMeta <- extractHTMLMeta uri content
                liftIO (runIO $ readHtml def content) >>= \case
                  Left _ -> pure htmlMeta
                  Right pd -> pure $ htmlMeta <> extractPandocMtdt pd
              Nothing -> pure mempty
            _ -> pure mempty
