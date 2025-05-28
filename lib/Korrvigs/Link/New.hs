{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Link.New
  ( new,
    NewLink (..),
    nlEntry,
    nlOffline,
    ExtractedData (..),
    exSubs,
    exMtdt,
    exContent,
  )
where

import Conduit (throwM)
import Control.Arrow (first)
import Control.Exception hiding (try)
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
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
import Korrvigs.Utils (fromMaybeT, joinNull)
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
htmlMetas :: forall m. (MonadKorrvigs m) => Map Text (Text, Text -> m (Maybe Value))
htmlMetas =
  M.fromList
    [ -- Standard metadata
      ("author", extractList Authors),
      ("description", extractText Abstract),
      -- OpenGraph
      ("og:title", extractText Title),
      ("og:description", extractText Abstract),
      ("og:locale", (mtdtSqlName Language, pure . extractLanguage)),
      -- ("og:image", undefined),
      -- Twitter
      ("twitter:title", extractText Title),
      ("twitter:description", extractText Abstract)
    ]
  where
    extractText :: (ExtraMetadata mtdt) => mtdt -> (Text, Text -> m (Maybe Value))
    extractText mtdt = (mtdtSqlName mtdt, pure . Just . String)
    extractList :: (ExtraMetadata mtdt) => mtdt -> (Text, Text -> m (Maybe Value))
    extractList mtdt = (mtdtSqlName mtdt, pure . Just . Array . V.singleton . String)
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

data ExtractedData = ExtractedData
  { _exMtdt :: Map Text Value,
    _exContent :: Maybe Text,
    _exSubs :: [Id]
  }

makeLenses ''ExtractedData

instance Semigroup ExtractedData where
  ExtractedData m1 t1 s1 <> ExtractedData m2 t2 s2 =
    ExtractedData (m1 <> m2) (t1 <> t2) (s1 <> s2)

instance Monoid ExtractedData where
  mempty = ExtractedData M.empty Nothing []

fromMap :: Map Text Value -> ExtractedData
fromMap mtdt = ExtractedData mtdt Nothing []

ldToMeta :: LDJsonData -> ExtractedData
ldToMeta ld
  | ld ^. ldContext == "http://www.schema.org" =
      fromMap $
        M.fromList $
          maybe [] (\nm -> [(mtdtSqlName Title, String nm)]) (ld ^. ldName)
            ++ maybe [] (\auth -> [(mtdtSqlName Authors, Array $ V.singleton $ String auth)]) (ld ^. ldAuthor)
            ++ maybe [] (\pub -> [(mtdtSqlName Publisher, Array $ V.singleton $ String pub)]) (ld ^. ldPublisher)
            ++ maybe [] (\abt -> [(mtdtSqlName Abstract, String abt)]) (ld ^. ldAbout)
ldToMeta _ = mempty

extractHTMLMeta :: forall m. (MonadKorrvigs m) => Text -> Text -> m ExtractedData
extractHTMLMeta url txt =
  mconcat . mconcat
    <$> sequence
      [ pure $ matchTitle <$> divvy 2 1 html,
        pure $ matchLD <$> divvy 2 1 html,
        pure $ matchRSS <$> html,
        mapM matchMeta html,
        pure $ singleton matchLanguage
      ]
  where
    html = takeWhile (/= TagClose ("head" :: Text)) $ parseTags txt
    matchRSS :: Tag Text -> ExtractedData
    matchRSS (TagOpen "link" attrs)
      | any (\(k, v) -> k == "rel" && v == "alternate") attrs = fromMaybe mempty $ do
          tp <- find ((== "type") . fst) attrs
          guard $ snd tp == "application/rss+xml" || snd tp == "application/atom+xml"
          ref <- snd <$> find ((== "href") . fst) attrs
          guard $ T.length ref > 0
          let uri = if T.head ref == '/' then url <> T.tail ref else ref
          pure $ fromMap $ M.singleton (mtdtSqlName Feed) $ toJSON uri
    matchRSS _ = mempty
    matchMeta :: Tag Text -> m ExtractedData
    matchMeta (TagOpen "meta" attrs) = fromMaybeT mempty $ do
      (_, nm) <- hoistMaybe $ find (\(k, _) -> k == "name") attrs
      (_, content) <- hoistMaybe $ find (\(k, _) -> k == "content") attrs
      (mtdt, extractor) <- hoistMaybe $ M.lookup nm htmlMetas
      val <- lift (extractor content) >>= hoistMaybe
      pure $ fromMap $ M.singleton mtdt val
    matchMeta _ = pure mempty
    matchTitle :: [Tag Text] -> ExtractedData
    matchTitle [TagOpen "title" _, TagText title] =
      fromMap $ M.singleton (mtdtSqlName Title) $ String title
    matchTitle _ = mempty
    matchLD :: [Tag Text] -> ExtractedData
    matchLD [tagOpen, TagText json]
      | tagOpen ~== TagOpen ("script" :: Text) [("type", "application/ld+json")] =
          case eitherDecode (LEnc.encodeUtf8 $ LT.fromStrict json) of
            Left _ -> mempty
            Right ld -> ldToMeta ld
    matchLD _ = mempty
    matchLanguage :: ExtractedData
    matchLanguage =
      if "fr" `T.isSuffixOf` url
        then fromMap $ M.singleton (mtdtSqlName Language) (String "fr")
        else mempty

processPandocMtdt :: Map Text Value -> ExtractedData
processPandocMtdt = fromMap . M.fromList . mapMaybe process . M.toList
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

extractPandocMtdt :: Pandoc -> ExtractedData
extractPandocMtdt pd = setContent $ processPandocMtdt mtdt
  where
    (content, mtdt) = pdExtractMtdt pd
    setContent = if T.null content then id else exContent ?~ content

downloadInformation :: (MonadKorrvigs m) => Text -> m ExtractedData
downloadInformation uri = do
  req <- parseRequest $ T.unpack uri
  resp <- catchIO $ tryJust (guard . isHttpException) $ httpBS req
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
                catchIO $
                  runIO (readHtml def content) >>= \case
                    Left _ -> pure mempty
                    Right pd -> pure $ htmlMeta <> extractPandocMtdt pd
              Nothing -> pure mempty
            _ -> pure mempty

new :: (MonadKorrvigs m) => Text -> NewLink -> m Id
new url options = case parseURI (T.unpack url) of
  Nothing -> throwM $ KMiscError $ "Could not parse URL: " <> url
  Just uri -> do
    let protocol = T.pack $ uriScheme uri
    let link = T.pack $ uriToString id uri ""
    let parents = unId <$> options ^. nlEntry . neParents
    extracted <-
      if options ^. nlOffline
        then pure mempty
        else downloadInformation link
    let info = extracted ^. exMtdt
    let title = mplus (joinNull T.null $ options ^. nlEntry . neTitle) (M.lookup (mtdtSqlName Title) info >>= jsonAsText)
    dt <- useDate (options ^. nlEntry) Nothing
    let mtdt =
          useMtdt (options ^. nlEntry) $
            M.union
              (M.fromList (first CI.mk <$> options ^. nlEntry . neMtdt))
              (M.fromList $ first CI.mk <$> M.toList info)
              & at (mtdtName Title) .~ (toJSON <$> title)
    let mtdtJson = M.fromList $ first CI.foldedCase <$> M.toList mtdt
    let txt = extracted ^. exContent
    let json = LinkJSON protocol link mtdtJson dt Nothing Nothing txt parents
    idmk' <- applyNewEntry (options ^. nlEntry) (imk "link")
    let idmk = idmk' & idTitle .~ title
    i <- newId idmk
    rt <- linkJSONPath
    let jsonTT = linkJSONTreeType
    let content = encodingToLazyByteString . value $ toJSON json
    pth <- storeFile rt jsonTT Nothing (unId i <> ".json") $ FileLazy content
    syncFileOfKind pth Link
    forM_ (extracted ^. exSubs) $ \sub -> do
      subEntry <- load sub
      forM_ subEntry $ \subE -> updateParents subE [i] []
    pure i
