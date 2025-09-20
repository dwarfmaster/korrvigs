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
    _cljsMetadata :: Map Text Value,
    _cljsDate :: Maybe ZonedTime,
    _cljsDuration :: Maybe CalendarDiffTime,
    _cljsGeo :: Maybe Geometry,
    _cljsText :: Maybe Text,
    _cljsTitle :: Maybe Text,
    _cljsParents :: [Text]
  }

makeLenses ''CalJSON

instance FromJSON CalJSON where
  parseJSON (Object v) =
    CalJSON
      <$> v .: "server"
      <*> v .: "user"
      <*> v .: "calendar"
      <*> v .: "metadata"
      <*> v .:? "date"
      <*> v .:? "duration"
      <*> v .:? "geometry"
      <*> v .:? "textContent"
      <*> v .:? "title"
      <*> v .: "parents"
  parseJSON invalid =
    prependFailure "parsing calendar failed, " $ typeMismatch "Object" invalid

instance ToJSON CalJSON where
  toJSON (CalJSON srv user nm mtdt dt dur geo txt title prts) =
    object $
      [ "server" .= srv,
        "user" .= user,
        "calendar" .= nm,
        "metadata" .= mtdt,
        "parents" .= prts
      ]
        ++ maybe [] ((: []) . ("date" .=)) dt
        ++ maybe [] ((: []) . ("duration" .=)) dur
        ++ maybe [] ((: []) . ("geometry" .=)) geo
        ++ maybe [] ((: []) . ("textContent" .=)) txt
        ++ maybe [] ((: []) . ("title" .=)) title

calJSONPath :: (MonadKorrvigs m) => m FilePath
calJSONPath = joinPath . (: ["calendars"]) <$> root
