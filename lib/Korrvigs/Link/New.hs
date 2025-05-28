{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Link.New (new, NewLink (..), nlEntry, nlOffline) where

import Conduit (throwM)
import Control.Arrow (first, second)
import Control.Exception hiding (try)
import Control.Lens hiding (noneOf)
import Control.Monad
import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString, value)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.List
import Data.List.Split (divvy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import qualified Data.Vector as V
import Korrvigs.Actions
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.Kind
import Korrvigs.Link.JSON
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Utils (joinNull)
import Korrvigs.Utils.DateTree
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Pandoc (pdExtractMtdt)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.Mime
import Network.URI
import Text.HTML.TagSoup
import Text.Pandoc hiding (Link, getCurrentTimeZone)
import Text.Parsec

data NewLink = NewLink
  { _nlEntry :: NewEntry,
    _nlOffline :: Bool
  }

makeLenses ''NewLink

instance Default NewLink where
  def = NewLink def False

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

-- Map property name to metadata name and extractor
htmlMetas :: Map Text (Text, Text -> Maybe Value)
htmlMetas =
  M.fromList
    [ -- Standard metadata
      ("author", extractList Authors),
      ("description", extractText Abstract),
      -- OpenGraph
      ("og:title", extractText Title),
      ("og:description", extractText Abstract),
      ("og:locale", (mtdtSqlName Language, extractLanguage)),
      -- ("og:image", undefined),
      -- Twitter
      ("twitter:title", extractText Title),
      ("twitter:description", extractText Abstract)
    ]
  where
    extractText :: (ExtraMetadata m) => m -> (Text, Text -> Maybe Value)
    extractText mtdt = (mtdtSqlName mtdt, Just . String)
    extractList :: (ExtraMetadata m) => m -> (Text, Text -> Maybe Value)
    extractList mtdt = (mtdtSqlName mtdt, Just . Array . V.singleton . String)
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

ldToMeta :: LDJsonData -> Map Text Value
ldToMeta ld
  | ld ^. ldContext == "http://www.schema.org" =
      M.fromList $
        maybe [] (\nm -> [(mtdtSqlName Title, String nm)]) (ld ^. ldName)
          ++ maybe [] (\auth -> [(mtdtSqlName Authors, Array $ V.singleton $ String auth)]) (ld ^. ldAuthor)
          ++ maybe [] (\pub -> [(mtdtSqlName Publisher, Array $ V.singleton $ String pub)]) (ld ^. ldPublisher)
          ++ maybe [] (\abt -> [(mtdtSqlName Abstract, String abt)]) (ld ^. ldAbout)
ldToMeta _ = M.empty

extractHTMLMeta :: Text -> Text -> Map Text Value
extractHTMLMeta url txt =
  mconcat $
    mconcat
      [ matchTitle <$> divvy 2 1 html,
        matchLD <$> divvy 2 1 html,
        matchRSS <$> html,
        matchMeta <$> html,
        singleton matchLanguage
      ]
  where
    html = takeWhile (/= TagClose ("head" :: Text)) $ parseTags txt
    matchRSS :: Tag Text -> Map Text Value
    matchRSS (TagOpen "link" attrs)
      | any (\(k, v) -> k == "rel" && v == "alternate") attrs = fromMaybe M.empty $ do
          tp <- find ((== "type") . fst) attrs
          guard $ snd tp == "application/rss+xml" || snd tp == "application/atom+xml"
          ref <- snd <$> find ((== "href") . fst) attrs
          guard $ T.length ref > 0
          let uri = if T.head ref == '/' then url <> T.tail ref else ref
          pure $ M.singleton (mtdtSqlName Feed) $ toJSON uri
    matchRSS _ = M.empty
    matchMeta :: Tag Text -> Map Text Value
    matchMeta (TagOpen "meta" attrs) = fromMaybe M.empty $ do
      (_, nm) <- find (\(k, _) -> k == "name") attrs
      (_, content) <- find (\(k, _) -> k == "content") attrs
      (mtdt, extractor) <- M.lookup nm htmlMetas
      val <- extractor content
      pure $ M.singleton mtdt val
    matchMeta _ = M.empty
    matchTitle :: [Tag Text] -> Map Text Value
    matchTitle [TagOpen "title" _, TagText title] =
      M.singleton (mtdtSqlName Title) $ String title
    matchTitle _ = M.empty
    matchLD :: [Tag Text] -> Map Text Value
    matchLD [tagOpen, TagText json]
      | tagOpen ~== TagOpen ("script" :: Text) [("type", "application/ld+json")] =
          case eitherDecode (LEnc.encodeUtf8 $ LT.fromStrict json) of
            Left _ -> M.empty
            Right ld -> ldToMeta ld
    matchLD _ = M.empty
    matchLanguage :: Map Text Value
    matchLanguage =
      if "fr" `T.isSuffixOf` url
        then M.singleton (mtdtSqlName Language) (String "fr")
        else M.empty

processPandocMtdt :: Map Text Value -> Map Text Value
processPandocMtdt = M.fromList . mapMaybe process . M.toList
  where
    process :: (Text, Value) -> Maybe (Text, Value)
    process (key, val) = (,val) <$> M.lookup key pdMtdt
    pdMtdt :: Map Text Text
    pdMtdt =
      M.fromList
        [ mkMtdt Title "title",
          mkMtdt Authors "authors",
          mkMtdt Abstract "abstract"
        ]
    mkMtdt :: (ExtraMetadata mtdt) => mtdt -> Text -> (Text, Text)
    mkMtdt mtdt pd = (pd, mtdtSqlName mtdt)

downloadInformation :: Text -> IO (Text, Map Text Value)
downloadInformation uri = do
  req <- parseRequest $ T.unpack uri
  resp <- tryJust (guard . isHttpException) $ httpBS req
  case resp of
    Left _ -> pure ("", M.empty)
    Right response -> do
      let status = getResponseStatus response
      if statusCode status /= 200
        then pure ("", M.empty)
        else do
          let ct = getResponseHeader "Content-Type" response
          let r = parse contentTypeP "" <$> ct
          case r of
            (Right ("text/html", decoder)) : _ -> case decoder (getResponseBody response) of
              Just content ->
                let htmlMeta = extractHTMLMeta uri content
                 in runIO (readHtml def content) >>= \case
                      Left _ -> pure ("", htmlMeta)
                      Right pd -> pure $ second (\pdm -> htmlMeta `M.union` processPandocMtdt pdm) $ pdExtractMtdt pd
              Nothing -> pure ("", M.empty)
            _ -> pure ("", M.empty)

new :: (MonadKorrvigs m) => Text -> NewLink -> m Id
new url options = case parseURI (T.unpack url) of
  Nothing -> throwM $ KMiscError $ "Could not parse URL: " <> url
  Just uri -> do
    let protocol = T.pack $ uriScheme uri
    let link = T.pack $ uriToString id uri ""
    let parents = unId <$> options ^. nlEntry . neParents
    (textContent, info) <-
      if options ^. nlOffline
        then pure ("", M.empty)
        else catchIO $ downloadInformation link
    let title = mplus (joinNull T.null $ options ^. nlEntry . neTitle) (M.lookup (mtdtSqlName Title) info >>= jsonAsText)
    dt <- useDate (options ^. nlEntry) Nothing
    let mtdt =
          useMtdt (options ^. nlEntry) $
            M.union
              (M.fromList (first CI.mk <$> options ^. nlEntry . neMtdt))
              (M.fromList $ first CI.mk <$> M.toList info)
              & at (mtdtName Title) .~ (toJSON <$> title)
    let mtdtJson = M.fromList $ first CI.foldedCase <$> M.toList mtdt
    let txt = if T.null textContent then Nothing else Just textContent
    let json = LinkJSON protocol link mtdtJson dt Nothing Nothing txt parents
    idmk' <- applyNewEntry (options ^. nlEntry) (imk "link")
    let idmk = idmk' & idTitle .~ title
    i <- newId idmk
    rt <- linkJSONPath
    let jsonTT = linkJSONTreeType
    let content = encodingToLazyByteString . value $ toJSON json
    pth <- storeFile rt jsonTT Nothing (unId i <> ".json") $ FileLazy content
    syncFileOfKind pth Link
    pure i
