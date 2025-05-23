{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Link.New (new, NewLink (..), nlEntry, nlOffline) where

import Conduit (throwM)
import Control.Arrow (first, second)
import Control.Exception
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString, value)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
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
  charset <- option decUtf8 $ do
    void $ string "; charset="
    charset <- many1 $ noneOf ";"
    pure $ case CI.foldedCase (CI.mk charset) of
      "ascii" -> Enc.decodeASCII'
      "iso-8859-1" -> Just . Enc.decodeLatin1
      _ -> decUtf8
  pure (Enc.encodeUtf8 $ T.pack mime, charset)

isHttpException :: SomeException -> Bool
isHttpException e = isJust (fromException e :: Maybe HttpException)

extractHTMLMeta :: Text -> Text -> Map Text Value
extractHTMLMeta url txt = mconcat $ mapMaybe matchRSS html
  where
    html = parseTags txt
    matchRSS :: Tag Text -> Maybe (Map Text Value)
    matchRSS (TagOpen "link" attrs)
      | any (\(k, v) -> k == "rel" && v == "alternate") attrs = do
          tp <- find ((== "type") . fst) attrs
          guard $ snd tp == "application/rss+xml" || snd tp == "application/atom+xml"
          ref <- snd <$> find ((== "href") . fst) attrs
          guard $ T.length ref > 0
          let uri = if T.head ref == '/' then url <> T.tail ref else ref
          pure $ M.singleton (mtdtSqlName Feed) $ toJSON uri
    matchRSS _ = Nothing

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
                      Right pd -> pure $ second (`M.union` htmlMeta) $ pdExtractMtdt pd
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
    liftIO $ print $ options ^. nlEntry . neTitle
    let title = mplus (joinNull T.null $ options ^. nlEntry . neTitle) (M.lookup (mtdtSqlName Title) info >>= jsonAsText)
    dt <- useDate (options ^. nlEntry) Nothing
    let language :: Maybe Value = case uriAuthority uri of
          Just auth | ".fr" `isSuffixOf` uriRegName auth -> Just "fr"
          _ -> Nothing
    let mtdt =
          useMtdt (options ^. nlEntry) $
            M.fromList (first CI.mk <$> options ^. nlEntry . neMtdt)
              & at (mtdtName Title) .~ (toJSON <$> title)
              & at "meta" ?~ toJSON (foldr M.delete info ["day", "duration", "geometry", "textContent"])
              & maybe id (at (mtdtName Abstract) ?~) (M.lookup "description" info)
              & maybe id (at (mtdtName Feed) ?~) (M.lookup (mtdtSqlName Feed) info)
              & maybe id (at (mtdtName Language) ?~) language
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
