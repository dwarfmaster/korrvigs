module Data.Viking.Data where

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)

data VikingFile = VikingFile
  { _vikKeys :: Map Text Text,
    _vikTopLayers :: [VikingTopLayer]
  }
  deriving (Eq, Ord, Show)

data VikingTopLayer = VikingTopLayer
  { _vikTopKind :: Text,
    _vikTopName :: Text,
    _vikTopKeys :: Map Text Text,
    _vikLayers :: [VikingLayer]
  }
  deriving (Eq, Ord, Show)

data VikingLayer = VikingLayer
  { _vikLayerType :: Text,
    _vikLayerName :: Text,
    _vikLayerKeys :: Map Text Text,
    _vikLayerWaypoints :: [VikingWayPoint],
    _vikLayerTracks :: [VikingTrack]
  }
  deriving (Eq, Ord, Show)

data VikingWayPoint = VikingWayPoint
  { _vikWPName :: Text,
    _vikWPLat :: Double,
    _vikWPLon :: Double
  }
  deriving (Eq, Ord, Show)

data VikingTrack = VikingTrack
  { _vikTrackName :: Text,
    _vikTrackColor :: Text,
    _vikSegments :: [[VikingTrackPoint]]
  }
  deriving (Eq, Ord, Show)

data VikingTrackPoint = VikingTrackPoint
  { _vikTPLat :: Double,
    _vikTPLon :: Double
  }
  deriving (Eq, Ord, Show)

makeLenses ''VikingFile
makeLenses ''VikingTopLayer
makeLenses ''VikingLayer
makeLenses ''VikingWayPoint
makeLenses ''VikingTrack
makeLenses ''VikingTrackPoint
