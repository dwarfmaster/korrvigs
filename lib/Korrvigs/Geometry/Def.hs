{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Geometry.Def where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Functor.Identity
import Linear.V2
import Text.Parsec
import Text.Parsec.Number

type Point = V2 Double

type Path = [Point]

data Polygon = Polygon Path [Path]
  deriving (Show, Eq, Ord)

makeLenses ''Polygon

data Geometry
  = GeoPoint Point
  | GeoPath Path
  | GeoPolygon Polygon
  deriving (Show, Eq, Ord)

makeLenses ''Geometry

instance (ToJSON a) => ToJSON (V2 a) where
  toJSON (V2 x y) =
    object
      [ "x" .= x,
        "y" .= y
      ]

instance (FromJSON a) => FromJSON (V2 a) where
  parseJSON = withObject "V2" $ \obj -> V2 <$> obj .: "x" <*> obj .: "y"

instance ToJSON Polygon where
  toJSON (Polygon p ps) =
    object
      [ "contour" .= p,
        "holes" .= ps
      ]

instance FromJSON Polygon where
  parseJSON = withObject "Polygon" $ \obj ->
    Polygon
      <$> obj .: "contour"
      <*> obj .: "holes"

mkRectangle :: Point -> Point -> Polygon
mkRectangle (V2 x1 y1) (V2 x2 y2) =
  Polygon [V2 (max x1 x2) (max y1 y2), V2 (max x1 x2) (min y1 y2), V2 (min x1 x2) (min y1 y2), V2 (min x1 x2) (max y1 y2)] []

pointP :: (Stream s Identity Char) => Parsec s u Point
pointP = V2 <$> floating2 True <*> (char ',' *> floating2 True)

rectangleP :: (Stream s Identity Char) => Parsec s u Polygon
rectangleP = mkRectangle <$> pointP <*> (char ':' *> pointP)
