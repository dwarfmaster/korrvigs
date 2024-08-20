{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Geometry.Def where

import Control.Lens.TH (makeLenses)
import Linear.V2

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
