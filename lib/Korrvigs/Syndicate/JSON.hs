module Korrvigs.Syndicate.JSON where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Geometry
import Korrvigs.Monad
import Korrvigs.Syndicate.Item
import System.FilePath (joinPath)

data SyndicateJSON = SyndicateJSON
  { _synjsUrl :: Maybe Text,
    _synjsETag :: Maybe Text,
    _synjsFilter :: Maybe (Id, Text),
    _synjsExpiration :: Maybe UTCTime,
    _synjsItems :: [SyndicatedItem],
    _synjsMetadata :: Map Text Value,
    _synjsDate :: Maybe ZonedTime,
    _synjsDuration :: Maybe CalendarDiffTime,
    _synjsGeo :: Maybe Geometry,
    _synjsText :: Maybe Text,
    _synjsTitle :: Maybe Text,
    _synjsParents :: [Text]
  }

makeLenses ''SyndicateJSON

parseFilter :: Value -> Parser (Id, Text)
parseFilter = withObject "SyndicateJSON filter" $ \obj ->
  (,) . MkId
    <$> obj .: "entry"
    <*> obj .: "code"

instance FromJSON SyndicateJSON where
  parseJSON = withObject "SyndicateJSON" $ \obj ->
    SyndicateJSON
      <$> obj .:? "url"
      <*> obj .:? "etag"
      <*> (obj .:? "filter" >>= mapM parseFilter)
      <*> obj .:? "expiration"
      <*> obj .: "items"
      <*> obj .: "metadata"
      <*> obj .:? "date"
      <*> obj .:? "duration"
      <*> obj .:? "geometry"
      <*> obj .:? "textContent"
      <*> obj .:? "title"
      <*> obj .: "parents"

renderFilter :: (Id, Text) -> Value
renderFilter (entry, code) = object ["entry" .= unId entry, "code" .= code]

instance ToJSON SyndicateJSON where
  toJSON (SyndicateJSON url etag flt expiration items mtdt dt dur geo txt title prts) =
    object $
      [ "items" .= items,
        "metadata" .= mtdt,
        "parents" .= prts
      ]
        ++ maybe [] ((: []) . ("url" .=)) url
        ++ maybe [] ((: []) . ("filter" .=) . renderFilter) flt
        ++ maybe [] ((: []) . ("etag" .=)) etag
        ++ maybe [] ((: []) . ("expiration" .=)) expiration
        ++ maybe [] ((: []) . ("date" .=)) dt
        ++ maybe [] ((: []) . ("duration" .=)) dur
        ++ maybe [] ((: []) . ("geometry" .=)) geo
        ++ maybe [] ((: []) . ("textContent" .=)) txt
        ++ maybe [] ((: []) . ("title" .=)) title

synJSONPath :: (MonadKorrvigs m) => m FilePath
synJSONPath = joinPath . (: ["syndicate"]) <$> root
