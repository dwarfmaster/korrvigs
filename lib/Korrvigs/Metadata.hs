module Korrvigs.Metadata
  ( MtdtExtras,
    mtdtGeometry,
    mtdtDate,
    mtdtDuration,
    mtdtParents,
    mtdtExtras,
    mtdtText,
    mtdtTitle,
  )
where

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
    _mtdtText :: Maybe Text,
    _mtdtTitle :: Maybe Text
  }

makeLenses ''MtdtExtras

instance Default MtdtExtras where
  def = MtdtExtras Nothing Nothing Nothing Nothing Nothing Nothing

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

extractTitle :: Map Text Value -> Maybe Text
extractTitle mtdt = M.lookup "title" mtdt >>= asText

mtdtExtras :: Map Text Value -> MtdtExtras
mtdtExtras mtdt =
  MtdtExtras
    { _mtdtGeometry = extractGeometry mtdt,
      _mtdtDate = dt,
      _mtdtDuration = dur,
      _mtdtParents = extractParents mtdt,
      _mtdtText = extractText mtdt,
      _mtdtTitle = extractTitle mtdt
    }
  where
    (dt, dur) = extractDate mtdt

type Inserter = Text -> Value -> Metadata -> Metadata

insertGeom :: Inserter -> Geometry -> Metadata -> Metadata
insertGeom ins geom = ins "geometry" $ toJSON $ decodeUtf8 $ writeGeometry geom

insertDate :: Inserter -> ZonedTime -> Metadata -> Metadata
insertDate ins dt = ins "date" $ toJSON $ formatShow iso8601Format dt

insertDuration :: Inserter -> CalendarDiffTime -> Metadata -> Metadata
insertDuration ins dur = ins "duration" $ toJSON $ formatShow durationTimeFormat dur

insertParents :: Inserter -> [Id] -> Metadata -> Metadata
insertParents ins pars = ins "parents" $ toJSON $ unId <$> pars

insertText :: Inserter -> Text -> Metadata -> Metadata
insertText ins txt = ins "textContent" $ toJSON txt

insertTitle :: Inserter -> Text -> Metadata -> Metadata
insertTitle ins title = ins "title" $ toJSON title

reifyMetadataImpl :: Inserter -> MtdtExtras -> Metadata -> Metadata
reifyMetadataImpl ins ex =
  maybe id (insertGeom ins) (ex ^. mtdtGeometry)
    . maybe id (insertDate ins) (ex ^. mtdtDate)
    . maybe id (insertDuration ins) (ex ^. mtdtDuration)
    . maybe id (insertParents ins) (ex ^. mtdtParents)
    . maybe id (insertText ins) (ex ^. mtdtText)
    . maybe id (insertTitle ins) (ex ^. mtdtTitle)
