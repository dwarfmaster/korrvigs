module Korrvigs.Metadata (mtdtGeometry, mtdtDate, reifyMetadata) where

import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (CalendarDiffTime, ZonedTime)
import Data.Time.Format.ISO8601
import Korrvigs.Entry
import Korrvigs.Geometry
import Korrvigs.Geometry.WKB (readGeometry, writeGeometry)

asText :: Value -> Maybe Text
asText (String txt) = Just txt
asText _ = Nothing

mtdtGeometry :: Metadata -> Maybe Geometry
mtdtGeometry mtdt =
  readGeometry . encodeUtf8 . encodeToLazyText . asText <$> M.lookup "geometry" mtdt

mtdtDate :: Metadata -> (Maybe ZonedTime, Maybe CalendarDiffTime)
mtdtDate mtdt = mmaybe $ do
  dtT <- M.lookup "date" mtdt >>= asText
  dt <- formatParseM iso8601Format $ T.unpack dtT
  let dur = M.lookup "duration" mtdt >>= asText >>= formatParseM durationTimeFormat . T.unpack
  pure (dt, dur)
  where
    mmaybe :: Maybe (a, Maybe b) -> (Maybe a, Maybe b)
    mmaybe Nothing = (Nothing, Nothing)
    mmaybe (Just (a, b)) = (Just a, b)

insertGeom :: Geometry -> Metadata -> Metadata
insertGeom geom = M.insert "geometry" $ toJSON $ decodeUtf8 $ writeGeometry geom

insertDate :: ZonedTime -> Metadata -> Metadata
insertDate dt = M.insert "date" $ toJSON $ formatShow iso8601Format dt

insertDuration :: CalendarDiffTime -> Metadata -> Metadata
insertDuration dur = M.insert "duration" $ toJSON $ formatShow durationTimeFormat dur

reifyMetadata :: Maybe Geometry -> Maybe ZonedTime -> Maybe CalendarDiffTime -> Metadata -> Metadata
reifyMetadata geom dt dur = maybe id insertGeom geom . maybe id insertDate dt . maybe id insertDuration dur
