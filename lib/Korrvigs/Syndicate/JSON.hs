module Korrvigs.Syndicate.JSON where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime
import Data.Vector ((!))
import qualified Data.Vector as V
import Korrvigs.Entry
import Korrvigs.Geometry
import Korrvigs.Monad
import Korrvigs.Syndicate.Item
import System.FilePath (joinPath)

data SyndicateJSON = SyndicateJSON
  { _synjsUrl :: Maybe Text,
    _synjsETag :: Maybe Text,
    _synjsFilters :: [(Id, Text)],
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
parseFilter = withArray "SyndicateJSON filter" $ \arr -> do
  guard $ V.length arr == 2
  i <- parseJSON $ arr ! 0
  code <- parseJSON $ arr ! 1
  pure (MkId i, code)

parseFilters :: Maybe [Value] -> Parser [(Id, Text)]
parseFilters Nothing = pure []
parseFilters (Just vs) = mapM parseFilter vs

instance FromJSON SyndicateJSON where
  parseJSON = withObject "SyndicateJSON" $ \obj ->
    SyndicateJSON
      <$> obj .:? "url"
      <*> obj .:? "etag"
      <*> (obj .:? "filters" >>= parseFilters)
      <*> obj .:? "expiration"
      <*> obj .: "items"
      <*> obj .: "metadata"
      <*> obj .:? "date"
      <*> obj .:? "duration"
      <*> obj .:? "geometry"
      <*> obj .:? "textContent"
      <*> obj .:? "title"
      <*> obj .: "parents"

filterToJSON :: (Id, Text) -> Value
filterToJSON (entry, code) = Array $ V.fromList $ toJSON <$> [unId entry, code]

instance ToJSON SyndicateJSON where
  toJSON (SyndicateJSON url etag flt expiration items mtdt dt dur geo txt title prts) =
    object $
      [ "items" .= items,
        "metadata" .= mtdt,
        "parents" .= prts
      ]
        ++ maybe [] ((: []) . ("url" .=)) url
        ++ ["filters" .= (filterToJSON <$> flt) | not (null flt)]
        ++ maybe [] ((: []) . ("etag" .=)) etag
        ++ maybe [] ((: []) . ("expiration" .=)) expiration
        ++ maybe [] ((: []) . ("date" .=)) dt
        ++ maybe [] ((: []) . ("duration" .=)) dur
        ++ maybe [] ((: []) . ("geometry" .=)) geo
        ++ maybe [] ((: []) . ("textContent" .=)) txt
        ++ maybe [] ((: []) . ("title" .=)) title

synJSONPath :: (MonadKorrvigs m) => m FilePath
synJSONPath = joinPath . (: ["syndicate"]) <$> root
