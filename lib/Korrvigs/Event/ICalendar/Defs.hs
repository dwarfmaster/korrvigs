module Korrvigs.Event.ICalendar.Defs where

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)

data ICalValue = ICValue
  { _icParams :: Map Text [Text],
    _icValue :: Text
  }
  deriving (Eq, Show)

makeLenses ''ICalValue

data ICalType
  = VEVENT
  | VTIMEZONE
  | VOTHER Text
  deriving (Eq, Show)

data ICalGroup = ICGroup
  { _icType :: ICalType,
    _icValues :: Map Text ICalValue,
    _icSubGroups :: [ICalGroup]
  }
  deriving (Eq, Show)

makeLenses ''ICalGroup

data ICalFile = ICFile
  { _icVersion :: Text,
    _icGroups :: [ICalGroup],
    _icOther :: Map Text ICalValue
  }
  deriving (Eq, Show)

makeLenses ''ICalFile
