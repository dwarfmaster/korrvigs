{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Link.New (new, NewLink (..), nlOffline, nlDate, nlTitle, nlParent) where

import Conduit (throwM)
import Control.Lens hiding (noneOf)
import Control.Monad
import Data.Aeson hiding (json)
import Data.Aeson.Encoding (encodingToLazyByteString, value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time.Calendar
import Korrvigs.Entry
import Korrvigs.KindData
import Korrvigs.Link.JSON
import Korrvigs.Link.Sync
import Korrvigs.Monad
import Korrvigs.Utils.DateTree
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Pandoc (pdExtractMtdt)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.Mime
import Network.URI
import Text.Pandoc
import Text.Parsec

data NewLink = NewLink
  { _nlOffline :: Bool,
    _nlDate :: Maybe Day,
    _nlTitle :: Maybe Text,
    _nlParent :: Maybe Id
  }

makeLenses ''NewLink

instance Default NewLink where
  def = NewLink False Nothing Nothing Nothing

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

downloadInformation :: Text -> IO (Map Text Value)
downloadInformation uri = do
  req <- parseRequest $ T.unpack uri
  response <- httpBS req
  let status = getResponseStatus response
  when (statusCode status /= 200) $
    fail $
      "Failed to download \"" <> T.unpack uri <> "\": " <> BS8.toString (statusMessage status)
  let ct = getResponseHeader "Content-Type" response
  let r = parse contentTypeP "" <$> ct
  case r of
    (Right ("text/html", decoder)) : _ -> case decoder (getResponseBody response) of
      Just content ->
        runIO (readHtml def content) >>= \case
          Left _ -> pure M.empty
          Right pd -> pure $ pdExtractMtdt pd
      Nothing -> pure M.empty
    _ -> pure M.empty

new :: (MonadKorrvigs m) => Text -> NewLink -> m Id
new url options = case parseURI (T.unpack url) of
  Nothing -> throwM $ KMiscError $ "Could not parse URL: " <> url
  Just uri -> do
    let protocol = T.pack $ uriScheme uri
    let link = T.pack $ uriToString id uri ""
    let parents = maybeToList $ unId <$> options ^. nlParent
    info <-
      if options ^. nlOffline
        then pure M.empty
        else catchIO $ downloadInformation link
    let json = LinkJSON protocol link info parents
    let idmk =
          imk "link"
            & idTitle .~ (M.lookup "title" info >>= jsonAsText)
            & idParent .~ options ^. nlParent
    i <- newId idmk
    rt <- linkJSONPath
    let jsonTT = linkJSONTreeType
    let content = encodingToLazyByteString . value $ toJSON json
    pth <- storeFile rt jsonTT Nothing (unId i <> ".json") content
    relData <- dSyncOneImpl pth
    atomicInsertRelData i relData
    pure i
