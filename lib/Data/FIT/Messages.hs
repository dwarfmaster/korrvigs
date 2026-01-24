module Data.FIT.Messages where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Data.Time.Clock
import Data.Word

data FITFile
  = FITFileDevice -- 1
  | FITFileSettings -- 2
  | FITFileSport -- 3
  | FITFileActivity -- 4
  | FITFileWorkout -- 5
  | FITFileCourse -- 6
  | FITFileSchedules -- 7
  | FITFileWeight -- 9
  | FITFileTotals -- 10
  | FITFileGoals -- 11
  | FITFileBloodPressure -- 14
  | FITFileMonitoringA -- 15
  | FITFileActivitySummary -- 20
  | FITFileMonitoringDaily -- 28
  | FITFileMonitoringB -- 32
  | FITFileSegment -- 34
  | FITFileSegmentList -- 35
  | FITFileExdConfiguration -- 40
  | FITFileUnknown Word8
  deriving (Show, Eq, Ord)

data FITManufacturer
  = FITGarmin -- 1
  | FITSigma -- 70
  | FITManufacturerUnknown Word16
  deriving (Show, Eq, Ord)

data FITFileId = FITFileId
  { _fitFileIdType :: Maybe FITFile,
    _fitFileIdManufacturer :: Maybe FITManufacturer,
    _fitFileIdProduct :: Maybe Word16,
    _fitFileIdTimeCreated :: Maybe UTCTime,
    _fitFileIdProductName :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance Default FITFileId where
  def = FITFileId Nothing Nothing Nothing Nothing Nothing

data FITEvent
  = FITEventTimer -- 0
  | FITEventWorkout -- 3
  | FITEventWorkoutStep -- 4
  | FITEventPowerDown -- 5
  | FITEventPowerUp -- 6
  | FITEventOffCourse -- 7
  | FITEventSession -- 8
  | FITEventLap -- 9
  | FITEventCoursePoint -- 10
  | FITEventBattery -- 11
  | FITEventActivity -- 26
  | FITEventUnknown Word8
  deriving (Show, Eq, Ord)

data FITEventType
  = FITEvStart -- 0
  | FITEvStop -- 1
  | FITEvMarker -- 3
  | FITEvStopAll -- 4
  | FITEvTpUnknown Word8
  deriving (Show, Eq, Ord)

data FITSport
  = FITGeneric -- 0
  | FITRunning -- 1
  | FITCycling -- 2
  | FITSwimming -- 5
  | FITTraining -- 10
  | FITWalking -- 11
  | FITCrossCountrySkiing -- 12
  | FITAlpineSkiing -- 13
  | FITSnowboarding -- 14
  | FITRowing -- 15
  | FITMountaineering -- 16
  | FITPaddling -- 19
  | FITFlying -- 20
  | FITEBiking -- 21
  | FITBoating -- 23
  | FITDriving -- 24
  | FITRockClimbing -- 31
  | FITSailing -- 32
  | FITSnowShoeing -- 35
  | FITSnowMobiling -- 36
  | FITSurfing -- 38
  | FITKayaking -- 41
  | FITRafting -- 42
  | FITAll -- 254
  | FITSportUnknown Word8
  deriving (Show, Eq, Ord)

data FITSubSport
  = FITSubGeneric -- 0
  | FITSubIndoorCycling -- 6
  | FITSubRoad -- 7
  | FITSubMountain -- 8
  | FITSubDownhill -- 9
  | FITSubRecumbent -- 10
  | FITSubTrackCycling -- 13
  | FITSubWhitewater -- 41
  | FITSubAll -- 254
  | FITSubSportUnknown Word8
  deriving (Show, Eq, Ord)

data FITSession = FITSession
  { _fitSessionEvent :: Maybe FITEvent,
    _fitSessionEventType :: Maybe FITEventType,
    _fitSessionStartTime :: Maybe UTCTime,
    _fitSessionStartLat :: Maybe Double,
    _fitSessionStartLon :: Maybe Double,
    _fitSessionSport :: Maybe FITSport,
    _fitSessionSubSport :: Maybe FITSubSport,
    _fitSessionTotalElapsedTime :: Maybe DiffTime,
    _fitSessionTotalTimerTime :: Maybe DiffTime,
    _fitSessionTotalDistance :: Maybe Double, -- m
    _fitSessionTotalCalories :: Maybe Int, -- kcal
    _fitSessionAvgSpeed :: Maybe Double, -- m/s
    _fitSessionMaxSpeed :: Maybe Double, -- m/s
    _fitSessionAvgHearthRate :: Maybe Int, -- bpm
    _fitSessionMaxHearthRate :: Maybe Int, -- bpm
    _fitSessionTotalAscent :: Maybe Int, -- m
    _fitSessionTotalDescent :: Maybe Int, -- m
    _fitSessionAvgTemperature :: Maybe Int, -- °C
    _fitSessionMaxTemperature :: Maybe Int, -- °C
    _fitSessionTotalMovingTime :: Maybe DiffTime,
    _fitSessionMinHearthRate :: Maybe Int, -- bpm
    _fitSessionTimestamp :: Maybe UTCTime
  }
  deriving (Show, Eq, Ord)

instance Default FITSession where
  def =
    FITSession
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def

data FITLap = FITLap
  { _fitLapEvent :: Maybe FITEvent,
    _fitLapEventType :: Maybe FITEventType,
    _fitLapStartTime :: Maybe UTCTime,
    _fitLapStartLat :: Maybe Double,
    _fitLapStartLon :: Maybe Double,
    _fitLapEndLat :: Maybe Double,
    _fitLapEndLon :: Maybe Double,
    _fitLapTotalElapsedTime :: Maybe DiffTime,
    _fitLapTotalTimerTime :: Maybe DiffTime,
    _fitLapTotalDistance :: Maybe Double, -- m
    _fitLapTotalCalories :: Maybe Int, -- kcal
    _fitLapAvgSpeed :: Maybe Double, -- m/s
    _fitLapAvgHearthRate :: Maybe Int, -- bpm
    _fitLapAvgCadence :: Maybe Int, -- rpm
    _fitLapAvgPower :: Maybe Int, -- watts
    _fitLapTotalAscent :: Maybe Int, -- m
    _fitLapTotalDescent :: Maybe Int, -- m
    _fitLapTimestamp :: Maybe UTCTime
  }
  deriving (Show, Eq, Ord)

instance Default FITLap where
  def =
    FITLap
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def
      def

data FITRecord = FITRecord
  { _fitRecordPosLat :: Maybe Double,
    _fitRecordPosLon :: Maybe Double,
    _fitRecordAltitude :: Maybe Double, -- m
    _fitRecordHearthRate :: Maybe Int, -- bpm
    _fitRecordDistance :: Maybe Double, -- m
    _fitRecordSpeed :: Maybe Double, -- m/s
    _fitRecordGrade :: Maybe Double, -- %
    _fitRecordTemperature :: Maybe Int, -- °C
    _fitRecordCalories :: Maybe Int, -- kcal
    _fitRecordTimestamp :: Maybe UTCTime
  }
  deriving (Show, Eq, Ord)

instance Default FITRecord where
  def = FITRecord def def def def def def def def def def

data FITRecEvent = FITRecEvent
  { _fitEventEvent :: Maybe FITEvent,
    _fitEventEventType :: Maybe FITEventType,
    _fitEventData :: Maybe Word32,
    _fitEventGroup :: Maybe Word8,
    _fitEventTimestamp :: Maybe UTCTime
  }
  deriving (Show, Eq, Ord)

instance Default FITRecEvent where
  def = FITRecEvent def def def def def

data FITActivityType
  = FITManual
  | FITAutoMultiSport
  deriving (Show, Eq, Ord)

data FITActivity = FITActivity
  { _fitActivityNumSessions :: Maybe Int,
    _fitActivityType :: Maybe FITActivityType,
    _fitActivityEvent :: Maybe FITEvent,
    _fitActivityEventType :: Maybe FITEventType,
    _fitActivityTimestamp :: Maybe UTCTime
  }
  deriving (Show, Eq, Ord)

instance Default FITActivity where
  def = FITActivity def def def def def

data FITMessage
  = FitMsgFileId FITFileId
  | FitMsgSession FITSession
  | FitMsgLap FITLap
  | FitMsgRecord FITRecord
  | FitMsgEvent FITRecEvent
  | FitMsgActivity FITActivity
  deriving (Eq, Show, Ord)

makePrisms ''FITFile
makePrisms ''FITManufacturer
makeLenses ''FITFileId
makePrisms ''FITEvent
makePrisms ''FITEventType
makePrisms ''FITSport
makePrisms ''FITSubSport
makeLenses ''FITSession
makeLenses ''FITLap
makeLenses ''FITRecord
makeLenses ''FITRecEvent
makePrisms ''FITActivityType
makeLenses ''FITActivity
makePrisms ''FITMessage
