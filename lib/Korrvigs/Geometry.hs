{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Geometry
  ( Point,
    Path,
    Polygon (..),
    Geometry (..),
    SqlGeometry,
    sqlPoint,
    sqlPath,
    sqlPolygon,
    stAsText,
    stArea,
    stAzimuth,
    stDistance,
    stLength,
    stPerimeter,
    stProject,
    stCovers,
    stIntersects,
    pointP,
    rectangleP,
    mkRectangle,
  )
where

import Data.Aeson
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Geometry.Def
import Korrvigs.Geometry.SQL
import Korrvigs.Geometry.WKB

instance ToJSON Geometry where
  toJSON = toJSON . LEnc.decodeASCII . writeGeometry

instance FromJSON Geometry where
  parseJSON v = readGeometry . LEnc.encodeUtf8 <$> parseJSON v
