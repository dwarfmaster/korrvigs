module Korrvigs.Metadata (MtdtExtras, mtdtGeometry, mtdtDate, mtdtDuration, mtdtParents, mtdtExtras, reifyMetadata) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Default
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (CalendarDiffTime, ZonedTime)
import Data.Time.Format.ISO8601
import qualified Data.Vector as V
import Korrvigs.Entry
import Korrvigs.Geometry
import Korrvigs.Geometry.WKB (readGeometry, writeGeometry)

data MtdtExtras = MtdtExtras
  { _mtdtGeometry :: Maybe Geometry,
    _mtdtDate :: Maybe ZonedTime,
    _mtdtDuration :: Maybe CalendarDiffTime,
    _mtdtParents :: Maybe [Id]
  }

makeLenses ''MtdtExtras

instance Default MtdtExtras where
  def = MtdtExtras Nothing Nothing Nothing Nothing

asText :: Value -> Maybe Text
asText (String txt) = Just txt
asText _ = Nothing

extractGeometry :: Metadata -> Maybe Geometry
extractGeometry mtdt =
  readGeometry . encodeUtf8 . encodeToLazyText . asText <$> M.lookup "geometry" mtdt

extractDate :: Metadata -> (Maybe ZonedTime, Maybe CalendarDiffTime)
extractDate mtdt = mmaybe $ do
  dtT <- M.lookup "date" mtdt >>= asText
  dt <- formatParseM iso8601Format $ T.unpack dtT
  let dur = M.lookup "duration" mtdt >>= asText >>= formatParseM durationTimeFormat . T.unpack
  pure (dt, dur)
  where
    mmaybe :: Maybe (a, Maybe b) -> (Maybe a, Maybe b)
    mmaybe Nothing = (Nothing, Nothing)
    mmaybe (Just (a, b)) = (Just a, b)

extractParents :: Metadata -> Maybe [Id]
extractParents mtdt = do
  pars <- M.lookup "parents" mtdt >>= asList
  txts <- mapM asText pars
  pure $ MkId <$> txts
  where
    asList :: Value -> Maybe [Value]
    asList (Array v) = Just $ V.toList v
    asList _ = Nothing

mtdtExtras :: Metadata -> MtdtExtras
mtdtExtras mtdt =
  MtdtExtras
    { _mtdtGeometry = extractGeometry mtdt,
      _mtdtDate = dt,
      _mtdtDuration = dur,
      _mtdtParents = extractParents mtdt
    }
  where
    (dt, dur) = extractDate mtdt

insertGeom :: Geometry -> Metadata -> Metadata
insertGeom geom = M.insert "geometry" $ toJSON $ decodeUtf8 $ writeGeometry geom

insertDate :: ZonedTime -> Metadata -> Metadata
insertDate dt = M.insert "date" $ toJSON $ formatShow iso8601Format dt

insertDuration :: CalendarDiffTime -> Metadata -> Metadata
insertDuration dur = M.insert "duration" $ toJSON $ formatShow durationTimeFormat dur

insertParents :: [Id] -> Metadata -> Metadata
insertParents pars = M.insert "parents" $ toJSON $ unId <$> pars

reifyMetadata :: MtdtExtras -> Metadata -> Metadata
reifyMetadata ex =
  maybe id insertGeom (ex ^. mtdtGeometry)
    . maybe id insertDate (ex ^. mtdtDate)
    . maybe id insertDuration (ex ^. mtdtDuration)
    . maybe id insertParents (ex ^. mtdtParents)
