module Korrvigs.Event.ICalendar.Defs where

import Control.Lens
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.LocalTime

data ICalValue a = ICValue
  { _icParams :: Map Text [Text],
    _icValue :: a
  }
  deriving (Eq, Show)

data ICalAbstractGroup = ICAGroup
  { _icValues :: Map Text [ICalValue Text],
    _icGroups :: Map Text [ICalAbstractGroup]
  }
  deriving (Eq, Show)

instance Default ICalAbstractGroup where
  def = ICAGroup def def

makeLenses ''ICalAbstractGroup
makeLenses ''ICalValue

-- Recurrence rules
data ICalFreq
  = Secondly
  | Minutely
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly
  deriving (Eq, Show)

data ICalRRule = ICRRule
  { _icrrFreq :: ICalFreq,
    _icrrUntil :: Maybe LocalTime,
    _icrrCount :: Maybe Int,
    _icrrInterval :: Maybe Int,
    _icrrBySec :: [Int],
    _icrrByMin :: [Int],
    _icrrByHour :: [Int],
    _icrrByDay :: [(Maybe Int, DayOfWeek)],
    _icrrByMonthDay :: [Int],
    _icrrByYearDay :: [Int],
    _icrrByWeekNo :: [Int],
    _icrrByMonth :: [Int],
    _icrrBySetPos :: [Int],
    _icrrWkst :: Maybe DayOfWeek
  }
  deriving (Eq, Show)

makeLenses ''ICalRRule

-- TimeZones

data ICalTZSpec = ICTZSpec
  { _ictzStandard :: Bool,
    _ictzStart :: LocalTime,
    _ictzOffsetTo :: Int,
    _ictzOffsetFrom :: Int,
    _ictzRdate :: Maybe LocalTime,
    _ictzName :: Maybe Text,
    _ictzRRule :: Maybe ICalRRule,
    _ictzContent :: ICalAbstractGroup
  }
  deriving (Eq, Show)

data ICalTimeZone = ICTZ
  { _ictzId :: Text,
    _ictzSpecs :: [ICalTZSpec],
    _ictzTopLevel :: ICalAbstractGroup
  }
  deriving (Eq, Show)

makeLenses ''ICalTZSpec
makeLenses ''ICalTimeZone

-- Events
-- TODO

-- Global structure
data ICalFile = ICFile
  { _icVersion :: Text,
    _icContent :: ICalAbstractGroup,
    _icTimezones :: Map Text ICalTimeZone
  }
  deriving (Eq, Show)

makeLenses ''ICalFile
