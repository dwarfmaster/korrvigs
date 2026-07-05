module Data.MBTiles where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Data.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Text.Read

data MBFormat
  = MBPng
  | MBJpg
  | MBPbf
  deriving (Eq, Ord, Show, Enum, Bounded)

data MBBounds = MBBounds
  { _mbMinLat :: Double,
    _mbMinLon :: Double,
    _mbMaxLat :: Double,
    _mbMaxLon :: Double
  }
  deriving (Eq, Ord, Show)

data MBPoint = MBPoint
  { _mbPtLon :: Double,
    _mbPtLat :: Double,
    _mbPtZoom :: Int
  }
  deriving (Eq, Ord, Show)

data MBMetadata = MBMetadata
  { _mbName :: Text,
    _mbFormat :: MBFormat,
    _mbMinZoom :: Maybe Int,
    _mbMaxZoom :: Maybe Int,
    _mbBounds :: Maybe MBBounds,
    _mbCenter :: Maybe MBPoint,
    _mbType :: Maybe Text,
    _mbDescription :: Maybe Text
  }
  deriving (Eq, Ord, Show)

newtype MBFile = MBFile Connection

makeLenses ''MBBounds
makeLenses ''MBPoint
makeLenses ''MBMetadata

openMBFile :: (MonadResource m, MonadIO m) => FilePath -> m MBFile
openMBFile path = MBFile . snd <$> allocate (open path) close

loadMetadata :: MBFile -> IO (Maybe MBMetadata)
loadMetadata (MBFile conn) = runMaybeT $ do
  nm <- loadMtdt' "name"
  format <-
    loadMtdt' "format" >>= \case
      "jpg" -> pure MBJpg
      "png" -> pure MBPng
      "pbf" -> pure MBPbf
      _ -> mzero
  minzoom <- loadMtdtV "minzoom"
  maxzoom <- loadMtdtV "maxzoom"
  bounds <- (parseBounds =<<) <$> loadMtdt "bounds"
  center <- (parsePoint =<<) <$> loadMtdt "center"
  tp <- loadMtdt "type"
  desc <- loadMtdt "description"
  pure $
    MBMetadata
      { _mbName = nm,
        _mbFormat = format,
        _mbMinZoom = minzoom,
        _mbMaxZoom = maxzoom,
        _mbBounds = bounds,
        _mbCenter = center,
        _mbType = tp,
        _mbDescription = desc
      }
  where
    loadMtdt :: (MonadIO m) => Text -> m (Maybe Text)
    loadMtdt nm =
      liftIO (query conn "SELECT value FROM metadata WHERE name=?" [nm]) >>= \case
        [Only v] -> pure $ Just v
        _ -> pure Nothing
    rm :: (Read a) => Text -> Maybe a
    rm = readMaybe . T.unpack
    loadMtdtV :: (MonadIO m, Read a) => Text -> m (Maybe a)
    loadMtdtV nm = (rm =<<) <$> loadMtdt nm
    loadMtdt' :: (MonadIO m) => Text -> MaybeT m Text
    loadMtdt' nm = loadMtdt nm >>= hoistMaybe
    parseBounds :: Text -> Maybe MBBounds
    parseBounds bds = case T.split (== ',') bds of
      [minlon, minlat, maxlon, maxlat] -> MBBounds <$> rm minlon <*> rm minlat <*> rm maxlon <*> rm maxlat
      _ -> Nothing
    parsePoint :: Text -> Maybe MBPoint
    parsePoint pt = case T.split (== ',') pt of
      [lon, lat, zm] -> MBPoint <$> rm lon <*> rm lat <*> rm zm
      _ -> Nothing

loadTile :: MBFile -> Int -> Int -> Int -> IO (Maybe ByteString)
loadTile (MBFile conn) lon lat zm = do
  query conn "SELECT tile_data FROM tiles WHERE zoom_level=? AND tile_column=? AND tile_row=?" (zm, lon, lat) >>= \case
    [Only r] -> pure $ Just r
    _ -> pure Nothing
