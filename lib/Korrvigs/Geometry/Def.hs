{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Geometry.Def where

import Control.Lens.TH (makeLenses)
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

mkRectangle :: Point -> Point -> Polygon
mkRectangle (V2 x1 y1) (V2 x2 y2) =
  Polygon [V2 (max x1 x2) (max y1 y2), V2 (max x1 x2) (min y1 y2), V2 (min x1 x2) (min y1 y2), V2 (min x1 x2) (max y1 y2)] []

pointP :: (Stream s Identity Char) => Parsec s u Point
pointP = V2 <$> floating2 True <*> (char ',' *> floating2 True)

rectangleP :: (Stream s Identity Char) => Parsec s u Polygon
rectangleP = mkRectangle <$> pointP <*> (char ':' *> pointP)
