module Korrvigs.Event.ICalendar.Defs where

import Control.Lens
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.LocalTime

data ICalValue = ICValue
  { _icParams :: Map Text [Text],
    _icValue :: Text
  }
  deriving (Eq, Show)

data ICalAbstractGroup = ICAGroup
  { _icValues :: Map Text [ICalValue],
    _icGroups :: Map Text [ICalAbstractGroup]
  }
  deriving (Eq, Show)

instance Default ICalAbstractGroup where
  def = ICAGroup def def

makeLenses ''ICalAbstractGroup
makeLenses ''ICalValue

-- Global structure
data ICalFile = ICFile
  { _icVersion :: Text,
    _icContent :: ICalAbstractGroup
  }
  deriving (Eq, Show)

makeLenses ''ICalFile
