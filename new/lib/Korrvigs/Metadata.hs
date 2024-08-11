module Korrvigs.Metadata (MtdtExtras, mtdtGeometry, mtdtDate, mtdtDuration, mtdtParents, mtdtExtras, mtdtText, reifyMetadata) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Default
import Data.Map (Map)
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
    _mtdtParents :: Maybe [Id],
    _mtdtText :: Maybe Text
  }

makeLenses ''MtdtExtras

instance Default MtdtExtras where
  def = MtdtExtras Nothing Nothing Nothing Nothing Nothing

asText :: Value -> Maybe Text
asText (String txt) = Just txt
asText _ = Nothing

extractGeometry :: Map Text Value -> Maybe Geometry
extractGeometry mtdt =
  readGeometry . encodeUtf8 . encodeToLazyText . asText <$> M.lookup "geometry" mtdt

extractDate :: Map Text Value -> (Maybe ZonedTime, Maybe CalendarDiffTime)
extractDate mtdt = mmaybe $ do
  dtT <- M.lookup "date" mtdt >>= asText
  dt <- formatParseM iso8601Format $ T.unpack dtT
  let dur = M.lookup "duration" mtdt >>= asText >>= formatParseM durationTimeFormat . T.unpack
  pure (dt, dur)
  where
    mmaybe :: Maybe (a, Maybe b) -> (Maybe a, Maybe b)
    mmaybe Nothing = (Nothing, Nothing)
    mmaybe (Just (a, b)) = (Just a, b)

extractParents :: Map Text Value -> Maybe [Id]
extractParents mtdt = do
  pars <- M.lookup "parents" mtdt >>= asList
  txts <- mapM asText pars
  pure $ MkId <$> txts
  where
    asList :: Value -> Maybe [Value]
    asList (Array v) = Just $ V.toList v
    asList _ = Nothing

extractText :: Map Text Value -> Maybe Text
extractText mtdt = M.lookup "textContent" mtdt >>= asText

mtdtExtras :: Map Text Value -> MtdtExtras
mtdtExtras mtdt =
  MtdtExtras
    { _mtdtGeometry = extractGeometry mtdt,
      _mtdtDate = dt,
      _mtdtDuration = dur,
      _mtdtParents = extractParents mtdt,
      _mtdtText = extractText mtdt
    }
  where
    (dt, dur) = extractDate mtdt

insertWithRO :: Text -> Value -> Metadata -> Metadata
insertWithRO key val mtdt = M.insert key (MValue val ro) mtdt
  where
    ro = maybe True (view metaReadOnly) $ M.lookup key mtdt

insertGeom :: Geometry -> Metadata -> Metadata
insertGeom geom = insertWithRO "geometry" $ toJSON $ decodeUtf8 $ writeGeometry geom

insertDate :: ZonedTime -> Metadata -> Metadata
insertDate dt = insertWithRO "date" $ toJSON $ formatShow iso8601Format dt

insertDuration :: CalendarDiffTime -> Metadata -> Metadata
insertDuration dur = insertWithRO "duration" $ toJSON $ formatShow durationTimeFormat dur

insertParents :: [Id] -> Metadata -> Metadata
insertParents pars = insertWithRO "parents" $ toJSON $ unId <$> pars

insertText :: Text -> Metadata -> Metadata
insertText txt = insertWithRO "textContent" $ toJSON txt

reifyMetadata :: MtdtExtras -> Metadata -> Metadata
reifyMetadata ex =
  maybe id insertGeom (ex ^. mtdtGeometry)
    . maybe id insertDate (ex ^. mtdtDate)
    . maybe id insertDuration (ex ^. mtdtDuration)
    . maybe id insertParents (ex ^. mtdtParents)
    . maybe id insertText (ex ^. mtdtText)
