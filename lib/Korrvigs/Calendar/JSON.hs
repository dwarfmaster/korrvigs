module Korrvigs.Calendar.JSON where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Geometry
import Korrvigs.Monad
import System.FilePath (joinPath)

data CalJSON = CalJSON
  { _cljsServer :: Text,
    _cljsUser :: Text,
    _cljsCalName :: Text,
    _cljsCalCache :: Text,
    _cljsMetadata :: Map Text Value,
    _cljsDate :: Maybe ZonedTime,
    _cljsDuration :: Maybe CalendarDiffTime,
    _cljsGeo :: Maybe Geometry,
    _cljsText :: Maybe Text,
    _cljsParents :: [Text]
  }

makeLenses ''CalJSON

instance FromJSON CalJSON where
  parseJSON (Object v) =
    CalJSON
      <$> v .: "server"
      <*> v .: "user"
      <*> v .: "calendar"
      <*> v .: "cache"
      <*> v .: "metadata"
      <*> v .:? "date"
      <*> v .:? "duration"
      <*> v .:? "geometry"
      <*> v .:? "textContent"
      <*> v .: "parents"
  parseJSON invalid =
    prependFailure "parsing calendar failed, " $ typeMismatch "Object" invalid

instance ToJSON CalJSON where
  toJSON (CalJSON srv user nm cache mtdt dt dur geo txt prts) =
    object $
      [ "server" .= srv,
        "user" .= user,
        "calendar" .= nm,
        "cache" .= cache,
        "metadata" .= mtdt,
        "parents" .= prts
      ]
        ++ maybe [] ((: []) . ("data" .=)) dt
        ++ maybe [] ((: []) . ("duration" .=)) dur
        ++ maybe [] ((: []) . ("geometry" .=)) geo
        ++ maybe [] ((: []) . ("textContent" .=)) txt

calJSONPath :: (MonadKorrvigs m) => m FilePath
calJSONPath = joinPath . (: ["calendars"]) <$> root
