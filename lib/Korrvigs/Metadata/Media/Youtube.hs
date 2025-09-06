module Korrvigs.Metadata.Media.Youtube where

import Conduit
import Control.Lens hiding (noneOf, parts)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.CaseInsensitive as CI
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Metadata.TH (mkMtdt)
import Korrvigs.Monad
import Korrvigs.Utils
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.URI as URI
import Network.URI
import Opaleye

mkMtdt "YoutubeVideoID" "youtube_video_id" [t|Text|]
mkMtdt "YoutubeChannelID" "youtube_channel_id" [t|Text|]

data YtRessource
  = YtVideo Text
  | YtChannelId Text
  | YtChannelHandle Text

parseQuery :: Text -> Maybe YtRessource
parseQuery url = do
  uri <- parseURI $ T.unpack url
  auth <- uriAuthority uri
  guard $ uriRegName auth == "www.youtube.com" || uriRegName auth == "youtube.com"
  case T.pack $ uriPath uri of
    "/watch" -> do
      let q = URI.parseQuery $ Enc.encodeUtf8 $ T.pack $ drop 1 $ uriQuery uri
      vidId <- join $ lookup "v" q
      pure $ YtVideo $ Enc.decodeUtf8 vidId
    channel -> case T.stripPrefix "/channel/" channel of
      Just i -> pure $ YtChannelId i
      Nothing -> YtChannelHandle <$> T.stripPrefix "/@" channel

data YtThumbnail = YtThumbnail
  { _thumbUrl :: Text,
    _thumbWidth :: Int,
    _thumbHeight :: Int
  }

data YtVideoItem = YtVideoItem
  { _vidTitle :: Text,
    _vidDescription :: Text,
    _vidChannelId :: Text,
    _vidThumbnails :: Map Text YtThumbnail,
    _vidPublishedAt :: Maybe UTCTime,
    _vidId :: Text,
    _vidDefaultLanguage :: Maybe Text,
    _vidDuration :: Maybe CalendarDiffTime
  }

data YtChannelItem = YtChannelItem
  { _chanId :: Text,
    _chanTitle :: Text,
    _chanDescription :: Text,
    _chanCustomUrl :: Maybe Text,
    _chanPublishedAt :: Maybe UTCTime,
    _chanThumbnails :: Map Text YtThumbnail,
    _chanCountry :: Maybe Text
  }

newtype YtResponse a = YtResponse
  { _respItems :: [a]
  }

makeLenses ''YtThumbnail
makeLenses ''YtVideoItem
makeLenses ''YtChannelItem
makeLenses ''YtResponse

instance FromJSON YtThumbnail where
  parseJSON = withObject "Youtube thumbnail" $ \obj ->
    YtThumbnail
      <$> obj .: "url"
      <*> obj .: "width"
      <*> obj .: "height"

parseUTC :: Text -> Parser UTCTime
parseUTC = formatParseM iso8601Format . T.unpack

parseDuration :: Text -> Parser CalendarDiffTime
parseDuration = formatParseM durationTimeFormat . T.unpack

instance FromJSON YtVideoItem where
  parseJSON = withObject "Youtube Video Item" $ \obj -> do
    snippet <- obj .: "snippet"
    details <- obj .: "contentDetails"
    YtVideoItem
      <$> snippet .: "title"
      <*> snippet .: "description"
      <*> snippet .: "channelId"
      <*> snippet .: "thumbnails"
      <*> (snippet .:? "publishedAt" >>= mapM parseUTC)
      <*> obj .: "id"
      <*> snippet .:? "defaultLanguage"
      <*> (details .:? "duration" >>= mapM parseDuration)

instance FromJSON YtChannelItem where
  parseJSON = withObject "Youtube Channel Item" $ \obj -> do
    snippet <- obj .: "snippet"
    YtChannelItem
      <$> obj .: "id"
      <*> snippet .: "title"
      <*> snippet .: "description"
      <*> snippet .:? "customUrl"
      <*> (snippet .:? "publishedAt" >>= mapM parseUTC)
      <*> snippet .: "thumbnails"
      <*> snippet .: "country"

instance (FromJSON a) => FromJSON (YtResponse a) where
  parseJSON = withObject "Youtube response" $ \obj -> YtResponse <$> obj .: "items"

doQueryYoutube :: (MonadKorrvigs m, FromJSON dat) => Text -> Text -> Text -> [Text] -> m dat
doQueryYoutube cred endpoint cond parts = do
  let url =
        mconcat
          [ "https://www.googleapis.com/youtube/v3/",
            endpoint,
            "?",
            cond,
            "&part=",
            T.intercalate "," parts,
            "&key=",
            cred
          ]
  req' <- parseRequest $ T.unpack url
  let req = req' {requestHeaders = ("User-Agent", "korrvigs v1.0") : requestHeaders req'}
  resp <- reqHttpM req
  case eitherDecode <$> resp of
    Nothing -> throwM $ KMiscError $ "Failed to get " <> url
    Just (Left err) -> throwM $ KMiscError $ "Failed to decode response (" <> url <> "): " <> T.pack err
    Just (Right vs) -> throwMaybe (KMiscError $ "No items returned from " <> url) $ listToMaybe $ vs ^. respItems

queryYoutube :: (MonadKorrvigs m) => YtRessource -> m (Maybe (NewEntry -> NewEntry))
queryYoutube ressource = do
  cred <- getCredential "youtube" >>= throwMaybe (KMiscError "No credential for Youtube")
  queryYoutubeCred cred ressource

selectCover :: Map Text YtThumbnail -> Maybe Text
selectCover = fmap (view thumbUrl) . listToMaybe . sortOn (Down . view thumbWidth) . M.elems

queryYoutubeCred :: (MonadKorrvigs m) => Text -> YtRessource -> m (Maybe (NewEntry -> NewEntry))
queryYoutubeCred cred (YtVideo vid) = do
  video <- doQueryYoutube cred "videos" ("id=" <> vid) ["snippet", "contentDetails"]
  let lang = case CI.mk <$> video ^. vidDefaultLanguage of
        Just "en" -> Just "en"
        Just "fr" -> Just "fr"
        _ -> Nothing
  let duration = fromRational . (/ 60) . toRational . ctTime <$> video ^. vidDuration
  parents <- rSelect $ do
    entry <- selectTable entriesTable
    tp <- baseSelectMtdt MediaMtdt $ entry ^. sqlEntryId
    where_ $ tp .== sqlValueJSONB (displayMediaType Channel)
    chan <- baseSelectTextMtdt YoutubeChannelID $ entry ^. sqlEntryId
    where_ $ chan .== sqlStrictText (video ^. vidChannelId)
    pure $ entry ^. sqlEntryName
  pure $
    Just $
      foldr
        (.)
        (setMtdtValue MediaMtdt Video)
        [ neTitle ?~ video ^. vidTitle,
          setMtdtValueM Abstract $ joinNull T.null $ Just $ video ^. vidDescription,
          maybe id (neDate ?~) $ video ^? vidPublishedAt . _Just . to utctDay,
          setMtdtValue YoutubeVideoID $ video ^. vidId,
          setMtdtValue YoutubeChannelID $ video ^. vidChannelId,
          setMtdtValue Url $ "https://youtube.com/watch?v=" <> video ^. vidId,
          setMtdtValueM Language lang,
          setMtdtValueM TimeEstimation duration,
          maybe id (neCover ?~) $ selectCover $ video ^. vidThumbnails,
          neParents %~ (parents ++)
        ]
queryYoutubeCred cred (YtChannelHandle handle) =
  queryYoutubeChannel cred ("forHandle=" <> handle)
queryYoutubeCred cred (YtChannelId i) =
  queryYoutubeChannel cred ("id=" <> i)

queryYoutubeChannel :: (MonadKorrvigs m) => Text -> Text -> m (Maybe (NewEntry -> NewEntry))
queryYoutubeChannel cred cond = do
  channel <- doQueryYoutube cred "channels" cond ["snippet"]
  let lang = case CI.mk <$> channel ^. chanCountry of
        Just "en" -> Just "en"
        Just "fr" -> Just "fr"
        _ -> Nothing
  let url = case channel ^. chanCustomUrl of
        Just custom -> "https://www.youtube.com/" <> custom
        Nothing -> "https://www.youtube.com/channel/" <> channel ^. chanId
  pure $
    Just $
      foldr
        (.)
        (setMtdtValue MediaMtdt Channel)
        [ neTitle ?~ channel ^. chanTitle,
          setMtdtValueM Abstract $ joinNull T.null $ Just $ channel ^. chanDescription,
          maybe id (neDate ?~) $ channel ^? chanPublishedAt . _Just . to utctDay,
          setMtdtValue YoutubeChannelID $ channel ^. chanId,
          setMtdtValue Url url,
          setMtdtValueM Language lang,
          maybe id (neCover ?~) $ selectCover $ channel ^. chanThumbnails,
          setMtdtValue Feed $ "https://www.youtube.com/feeds/videos.xml?channel_id=" <> channel ^. chanId,
          setMtdtValue HubMtdt "yes"
        ]
