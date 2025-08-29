module Korrvigs.Link.Download (downloadInformation) where

import Conduit (liftIO)
import Control.Exception hiding (try)
import Control.Lens hiding (noneOf)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Default
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
import qualified Data.Vector as V
import Korrvigs.Entry.New
import qualified Korrvigs.Link.Download.Manga as Manga
import qualified Korrvigs.Link.Download.Video as Vid
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Utils.Pandoc (pdExtractMtdt)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.Mime
import Text.HTML.TagSoup
import Text.Pandoc hiding (Link, getCurrentTimeZone)
import Text.Parsec hiding ((<|>))

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right r) = Just r

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

extractImage :: Text -> (Maybe Text, Maybe Value)
extractImage url = (Just url, Nothing)

-- Map property name to metadata name and extractor
htmlMetas :: Map Text (CI Text, Text -> (Maybe Text, Maybe Value))
htmlMetas =
  M.fromList
    [ -- Standard metadata
      ("author", extractList Authors),
      ("description", extractText Abstract),
      -- OpenGraph
      ("og:title", extractText Title),
      ("og:description", extractText Abstract),
      ("og:locale", (mtdtName Language, (Nothing,) . extractLanguage)),
      ("og:image", (mtdtName Cover, extractImage)),
      -- Twitter
      ("twitter:title", extractText Title),
      ("twitter:description", extractText Abstract)
    ]
  where
    extractText mtdt = (mtdtName mtdt, (Nothing,) . Just . String)
    extractList mtdt = (mtdtName mtdt, (Nothing,) . Just . Array . V.singleton . String)
    extractLanguage :: Text -> Maybe Value
    extractLanguage loc
      | "en_" `T.isPrefixOf` loc = Just "en"
      | "fr_" `T.isPrefixOf` loc = Just "fr"
      | otherwise = Nothing

data LDJsonData = LDJsonData
  { _ldContext :: Text,
    _ldName :: Maybe Text,
    _ldAuthor :: Maybe Text,
    _ldPublisher :: Maybe Text,
    _ldAbout :: Maybe Text
  }
  deriving (Show)

makeLenses ''LDJsonData

instance FromJSON LDJsonData where
  parseJSON = withObject "LDJsonData" $ \obj ->
    LDJsonData
      <$> obj .: "@context"
      <*> obj .:? "name"
      <*> obj .:? "author"
      <*> obj .:? "publisher"
      <*> obj .:? "about"

ldToMeta :: LDJsonData -> Endo NewEntry
ldToMeta ld
  | ld ^. ldContext == "http://www.schema.org" =
      mconcat $
        Endo
          <$> [ setMtdtValueM Title (ld ^. ldName),
                setMtdtValueM Authors (singleton <$> ld ^. ldAuthor),
                setMtdtValueM Publisher (singleton <$> ld ^. ldPublisher),
                setMtdtValueM Abstract (ld ^. ldAbout)
              ]
ldToMeta _ = mempty

extractHTMLMeta :: (MonadKorrvigs m) => Text -> Text -> m (Endo NewEntry)
extractHTMLMeta url txt =
  fmap mconcat . sequence $
    [ Vid.youtube url tags,
      Vid.nebula url tags,
      Manga.manytoon url tags,
      pure . mconcat $ matchTitle <$> divvy 2 1 html,
      pure . mconcat $ matchLD <$> divvy 2 1 html,
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
          let uri = if T.head ref == '/' then url <> T.tail ref else ref
          pure $ Endo $ setMtdtValue Feed uri
    matchRSS _ = mempty
    matchMeta :: Tag Text -> Endo NewEntry
    matchMeta (TagOpen "meta" attrs) = fromMaybe mempty $ do
      (_, nm) <- find (\(k, _) -> k == "property") attrs
      (_, content) <- find (\(k, _) -> k == "content") attrs
      (mtdt, extractor) <- M.lookup nm htmlMetas
      let (mcover, mval) = extractor content
      let excover = Endo $ maybe id (neCover ?~) mcover
      let exval = Endo $ maybe id (neMtdt . at mtdt ?~) mval
      pure $ excover <> exval
    matchMeta _ = mempty
    matchTitle :: [Tag Text] -> Endo NewEntry
    matchTitle [TagOpen "title" _, TagText title] =
      Endo $ setMtdtValue Title title
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
        then Endo $ setMtdtValue Language "fr"
        else mempty

processPandocMtdt :: Map Text Value -> Endo NewEntry
processPandocMtdt = mconcat . mapMaybe process . M.toList
  where
    process :: (Text, Value) -> Maybe (Endo NewEntry)
    process (key, val) = (\k -> Endo $ neMtdt . at (CI.mk k) ?~ val) <$> M.lookup key pdMtdt
    pdMtdt :: Map Text Text
    pdMtdt =
      M.fromList
        [ mkMtdt Title "title",
          mkMtdt Authors "authors",
          mkMtdt Abstract "abstract"
        ]
    mkMtdt :: (ExtraMetadata mtdt) => mtdt -> Text -> (Text, Text)
    mkMtdt mtdt pd = (pd, mtdtSqlName mtdt)

extractPandocMtdt :: Pandoc -> Endo NewEntry
extractPandocMtdt pd = Endo setContent <> processPandocMtdt mtdt
  where
    (content, mtdt) = pdExtractMtdt pd
    setContent = if T.null content then id else neContent ?~ content

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
