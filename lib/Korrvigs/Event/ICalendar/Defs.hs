module Korrvigs.Event.ICalendar.Defs where

import Control.Lens
import Data.Aeson
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Geometry

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
  deriving (Eq, Ord, Show)

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
    _ictzOffsetTo :: Int, -- In seconds
    _ictzOffsetFrom :: Int, -- In seconds
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

data ICalTimeSpec = ICTmSpec
  { _ictmDate :: LocalTime,
    _ictmUTC :: Bool,
    _ictmTimeZone :: Maybe Text
  }
  deriving (Eq, Show)

data ICalEvent = ICEvent
  { _iceUid :: Text, -- UID
    _iceCategories :: [Text], -- CATEGORIES
    _iceComment :: Maybe Text, -- COMMENT
    _iceSummary :: Maybe Text, -- SUMMARY
    _iceDescription :: Maybe Text, -- DESCRIPTION
    _iceLocation :: Maybe Text, -- LOCATION
    _iceStart :: Maybe ICalTimeSpec, -- DTSTART
    _iceEnd :: Maybe ICalTimeSpec, -- DTEND
    _iceDuration :: Maybe NominalDiffTime, -- DURATION
    _iceTransparent :: Bool, -- TRANSP
    _iceId :: Maybe Id, -- X-KORRVIGS-NAME
    _iceParents :: [Id], -- X-KORRVIGS-PARENTS
    _iceGeometry :: Maybe Geometry, -- GEO if point or X-KORRVIGS-GEOM
    _iceMtdt :: Map Text Value, -- X-KORRMTDT-*
    _iceContent :: ICalAbstractGroup
  }
  deriving (Eq, Show)

makeLenses ''ICalTimeSpec
makeLenses ''ICalEvent

-- Global structure
data ICalFile = ICFile
  { _icVersion :: Text,
    _icContent :: ICalAbstractGroup,
    _icTimezones :: Map Text ICalTimeZone,
    _icEvent :: Maybe ICalEvent
  }
  deriving (Eq, Show)

makeLenses ''ICalFile
