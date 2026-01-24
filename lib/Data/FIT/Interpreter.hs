module Data.FIT.Interpreter where

import Conduit
import Control.Lens
import Control.Monad.Loops
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.Default
import Data.FIT.Data
import Data.FIT.Messages
import Data.FIT.Parser
import Data.IORef
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Word
import Korrvigs.Utils

fitInterpret :: (MonadIO m) => Maybe FilePath -> ConduitT ByteString FITMessage m ()
fitInterpret path = fitParser path .| parseRecord

parseRecord :: (MonadIO m) => ConduitT FitRecord FITMessage m ()
parseRecord = do
  stateRef <- liftIO $ newIORef M.empty
  whileJust_ await $ \rec -> fromMaybeT () $ do
    -- We ignore compressed values since they are not used in our dataset
    normal <- hoistMaybe $ rec ^? _FitNormal
    let msgId = normal ^. fitRecLocalType
    state <- liftIO $ readIORef stateRef
    case normal ^. fitRecData of
      FitDefinition df -> liftIO $ writeIORef stateRef $ updateState msgId df state
      FitData dat ->
        maybe (pure ()) (lift . yield) $ parseValues state msgId dat

-- Parser structure
type ParserState = Map Word8 ([FitFieldValue] -> FITMessage)

makeFieldParser ::
  ((Maybe b -> Identity (Maybe b)) -> a -> Identity a) ->
  (FitFieldValue -> Maybe b) ->
  FitFieldValue ->
  Endo a
makeFieldParser ls parser value = case parser value of
  Nothing -> mempty
  Just v -> Endo $ ls ?~ v

makeParser :: (Default a) => (a -> FITMessage) -> Map Word8 (FitFieldValue -> Endo a) -> [Word8] -> [FitFieldValue] -> FITMessage
makeParser toMsg fields df values =
  toMsg $ appEndo (mconcat $ doParse <$> zip df values) def
  where
    doParse (d, v) = fromMaybe (const mempty) (M.lookup d fields) v

updateState :: Word8 -> FitDefinitionData -> ParserState -> ParserState
updateState msgId fitDef state =
  case M.lookup (fitDef ^. fitDefGlobalMsg) messageParsers of
    Nothing -> M.delete msgId state
    Just parser ->
      let parser' = parser $ fitDef ^.. fitDefData . each . fitFieldDefNumber
       in M.insert msgId parser' state

parseValues :: ParserState -> Word8 -> [FitFieldValue] -> Maybe FITMessage
parseValues state msgId values = ($ values) <$> M.lookup msgId state

-- High level parser
messageParsers :: Map Word16 ([Word8] -> [FitFieldValue] -> FITMessage)
messageParsers =
  M.fromList
    [ (0, makeParser FitMsgFileId fileIdFields),
      (18, makeParser FitMsgSession sessionFields),
      (19, makeParser FitMsgLap lapFields),
      (20, makeParser FitMsgRecord recordFields),
      (21, makeParser FitMsgEvent eventFields),
      (43, makeParser FitMsgActivity activityFields)
    ]

fileIdFields :: Map Word8 (FitFieldValue -> Endo FITFileId)
fileIdFields =
  M.fromList
    [ (0, makeFieldParser fitFileIdType parseFitFile),
      (1, makeFieldParser fitFileIdManufacturer parseFitManufacturer),
      (2, makeFieldParser fitFileIdProduct parseWord16),
      (4, makeFieldParser fitFileIdTimeCreated parseTimestamp),
      (8, makeFieldParser fitFileIdProductName parseText)
    ]

sessionFields :: Map Word8 (FitFieldValue -> Endo FITSession)
sessionFields =
  M.fromList
    [ (0, makeFieldParser fitSessionEvent parseFitEvent),
      (1, makeFieldParser fitSessionEventType parseFitEventType),
      (2, makeFieldParser fitSessionStartTime parseTimestamp),
      (3, makeFieldParser fitSessionStartLat parseLat),
      (4, makeFieldParser fitSessionStartLon parseLon),
      (5, makeFieldParser fitSessionSport parseFitSport),
      (6, makeFieldParser fitSessionSubSport parseFitSubSport),
      (7, makeFieldParser fitSessionTotalElapsedTime parseSeconds),
      (8, makeFieldParser fitSessionTotalTimerTime parseSeconds),
      (9, makeFieldParser fitSessionTotalDistance $ parseShifted32 100 0),
      (11, makeFieldParser fitSessionTotalCalories $ toInt . parseWord16),
      (14, makeFieldParser fitSessionAvgSpeed $ parseShifted16 1000 0),
      (15, makeFieldParser fitSessionMaxSpeed $ parseShifted16 1000 0),
      (16, makeFieldParser fitSessionAvgHearthRate $ toInt . parseWord8),
      (17, makeFieldParser fitSessionMaxHearthRate $ toInt . parseWord8),
      (22, makeFieldParser fitSessionTotalAscent $ toInt . parseWord16),
      (23, makeFieldParser fitSessionTotalDescent $ toInt . parseWord16),
      (57, makeFieldParser fitSessionAvgTemperature $ toInt . parseInt8),
      (58, makeFieldParser fitSessionMaxTemperature $ toInt . parseInt8),
      (59, makeFieldParser fitSessionTotalMovingTime parseSeconds),
      (64, makeFieldParser fitSessionMinHearthRate $ toInt . parseWord8),
      (253, makeFieldParser fitSessionTimestamp parseTimestamp)
    ]

lapFields :: Map Word8 (FitFieldValue -> Endo FITLap)
lapFields =
  M.fromList
    [ (0, makeFieldParser fitLapEvent parseFitEvent),
      (1, makeFieldParser fitLapEventType parseFitEventType),
      (2, makeFieldParser fitLapStartTime parseTimestamp),
      (3, makeFieldParser fitLapStartLat parseLat),
      (4, makeFieldParser fitLapStartLon parseLon),
      (5, makeFieldParser fitLapEndLat parseLat),
      (6, makeFieldParser fitLapEndLon parseLon),
      (7, makeFieldParser fitLapTotalElapsedTime parseSeconds),
      (8, makeFieldParser fitLapTotalTimerTime parseSeconds),
      (9, makeFieldParser fitLapTotalDistance $ parseShifted32 100 0),
      (11, makeFieldParser fitLapTotalCalories $ toInt . parseWord16),
      (13, makeFieldParser fitLapAvgSpeed $ parseShifted16 1000 0),
      (15, makeFieldParser fitLapAvgHearthRate $ toInt . parseWord8),
      (17, makeFieldParser fitLapAvgCadence $ toInt . parseWord8),
      (19, makeFieldParser fitLapAvgPower $ toInt . parseWord16),
      (21, makeFieldParser fitLapTotalAscent $ toInt . parseWord16),
      (22, makeFieldParser fitLapTotalDescent $ toInt . parseWord16),
      (253, makeFieldParser fitLapTimestamp parseTimestamp)
    ]

recordFields :: Map Word8 (FitFieldValue -> Endo FITRecord)
recordFields =
  M.fromList
    [ (0, makeFieldParser fitRecordPosLat parseLat),
      (1, makeFieldParser fitRecordPosLon parseLon),
      (2, makeFieldParser fitRecordAltitude $ parseShifted16 5 500),
      (3, makeFieldParser fitRecordHearthRate $ toInt . parseWord8),
      (5, makeFieldParser fitRecordDistance $ parseShifted32 100 0),
      (6, makeFieldParser fitRecordSpeed $ parseShifted16 1000 0),
      (9, makeFieldParser fitRecordGrade $ parseShifted16 100 0),
      (13, makeFieldParser fitRecordTemperature $ toInt . parseInt8),
      (33, makeFieldParser fitRecordCalories $ toInt . parseWord16),
      (253, makeFieldParser fitRecordTimestamp parseTimestamp)
    ]

eventFields :: Map Word8 (FitFieldValue -> Endo FITRecEvent)
eventFields =
  M.fromList
    [ (0, makeFieldParser fitEventEvent parseFitEvent),
      (1, makeFieldParser fitEventEventType parseFitEventType),
      (3, makeFieldParser fitEventData parseWord32),
      (4, makeFieldParser fitEventGroup parseWord8),
      (253, makeFieldParser fitEventTimestamp parseTimestamp)
    ]

activityFields :: Map Word8 (FitFieldValue -> Endo FITActivity)
activityFields =
  M.fromList
    [ (1, makeFieldParser fitActivityNumSessions $ toInt . parseWord16),
      (2, makeFieldParser fitActivityType parseFitActivity),
      (3, makeFieldParser fitActivityEvent parseFitEvent),
      (4, makeFieldParser fitActivityEventType parseFitEventType),
      (253, makeFieldParser fitActivityTimestamp parseTimestamp)
    ]

-- Low-level parsers
toInt :: (Integral a) => Maybe a -> Maybe Int
toInt = fmap $ fromInteger . toInteger

parseFitFile :: FitFieldValue -> Maybe FITFile
parseFitFile (FitEnumV [e]) = Just $ case e of
  1 -> FITFileDevice
  2 -> FITFileSettings
  3 -> FITFileSport
  4 -> FITFileActivity
  5 -> FITFileWorkout
  6 -> FITFileCourse
  7 -> FITFileSchedules
  9 -> FITFileWeight
  10 -> FITFileTotals
  11 -> FITFileGoals
  14 -> FITFileBloodPressure
  15 -> FITFileMonitoringA
  20 -> FITFileActivitySummary
  28 -> FITFileMonitoringDaily
  32 -> FITFileMonitoringB
  34 -> FITFileSegment
  35 -> FITFileSegmentList
  40 -> FITFileExdConfiguration
  _ -> FITFileUnknown e
parseFitFile _ = Nothing

parseFitManufacturer :: FitFieldValue -> Maybe FITManufacturer
parseFitManufacturer (FitUint16V [e]) = Just $ case e of
  1 -> FITGarmin
  70 -> FITSigma
  _ -> FITManufacturerUnknown e
parseFitManufacturer _ = Nothing

parseInt8 :: FitFieldValue -> Maybe Int8
parseInt8 (FitSint8V [w]) = Just w
parseInt8 _ = Nothing

parseWord8 :: FitFieldValue -> Maybe Word8
parseWord8 (FitUint8V [w]) = Just w
parseWord8 _ = Nothing

parseWord16 :: FitFieldValue -> Maybe Word16
parseWord16 (FitUint16V [w]) = Just w
parseWord16 _ = Nothing

parseWord32 :: FitFieldValue -> Maybe Word32
parseWord32 (FitUint32V [w]) = Just w
parseWord32 _ = Nothing

parseTimestamp :: FitFieldValue -> Maybe UTCTime
parseTimestamp (FitUint32V [ts]) =
  Just $ addUTCTime (fromInteger $ toInteger ts) timeZero
  where
    timeZero :: UTCTime
    timeZero = UTCTime (fromGregorian 1989 December 31) 0
parseTimestamp _ = Nothing

parseText :: FitFieldValue -> Maybe Text
parseText (FitStringV txt) = Just txt
parseText _ = Nothing

parseFitEvent :: FitFieldValue -> Maybe FITEvent
parseFitEvent (FitEnumV [e]) = Just $ case e of
  0 -> FITEventTimer
  3 -> FITEventWorkout
  4 -> FITEventWorkoutStep
  5 -> FITEventPowerDown
  6 -> FITEventPowerUp
  7 -> FITEventOffCourse
  8 -> FITEventSession
  9 -> FITEventLap
  10 -> FITEventCoursePoint
  11 -> FITEventBattery
  26 -> FITEventActivity
  _ -> FITEventUnknown e
parseFitEvent _ = Nothing

parseFitEventType :: FitFieldValue -> Maybe FITEventType
parseFitEventType (FitEnumV [e]) = Just $ case e of
  0 -> FITEvStart
  1 -> FITEvStop
  3 -> FITEvMarker
  4 -> FITEvStopAll
  _ -> FITEvTpUnknown e
parseFitEventType _ = Nothing

parseFitSport :: FitFieldValue -> Maybe FITSport
parseFitSport (FitEnumV [e]) = Just $ case e of
  0 -> FITGeneric
  1 -> FITRunning
  2 -> FITCycling
  5 -> FITSwimming
  10 -> FITTraining
  11 -> FITWalking
  12 -> FITCrossCountrySkiing
  13 -> FITAlpineSkiing
  14 -> FITSnowboarding
  15 -> FITRowing
  16 -> FITMountaineering
  19 -> FITPaddling
  20 -> FITFlying
  21 -> FITEBiking
  23 -> FITBoating
  24 -> FITDriving
  31 -> FITRockClimbing
  32 -> FITSailing
  35 -> FITSnowShoeing
  36 -> FITSnowMobiling
  38 -> FITSurfing
  41 -> FITKayaking
  42 -> FITRafting
  254 -> FITAll
  _ -> FITSportUnknown e
parseFitSport _ = Nothing

parseFitSubSport :: FitFieldValue -> Maybe FITSubSport
parseFitSubSport (FitEnumV [e]) = Just $ case e of
  0 -> FITSubGeneric
  6 -> FITSubIndoorCycling
  7 -> FITSubRoad
  8 -> FITSubMountain
  9 -> FITSubDownhill
  10 -> FITSubRecumbent
  13 -> FITSubTrackCycling
  41 -> FITSubWhitewater
  254 -> FITSubAll
  _ -> FITSubSportUnknown e
parseFitSubSport _ = Nothing

parseShifted :: (Num a, Integral a) => (FitFieldValue -> Maybe a) -> Double -> Double -> FitFieldValue -> Maybe Double
parseShifted parser factor offset value = apply <$> parser value
  where
    apply n = fromInteger (toInteger n) / factor - offset

parseShifted16 :: Double -> Double -> FitFieldValue -> Maybe Double
parseShifted16 = parseShifted parseWord16

parseShifted32 :: Double -> Double -> FitFieldValue -> Maybe Double
parseShifted32 = parseShifted parseWord32

parseSeconds :: FitFieldValue -> Maybe DiffTime
parseSeconds = fmap realToFrac . parseShifted32 1000 0

parseSemiCircle :: FitFieldValue -> Maybe Double
parseSemiCircle (FitSint32V [semi]) = Just $ realToFrac semi * 180 / (2 ** 31)
parseSemiCircle _ = Nothing

parseLat :: FitFieldValue -> Maybe Double
parseLat = parseSemiCircle

parseLon :: FitFieldValue -> Maybe Double
parseLon = parseSemiCircle

parseFitActivity :: FitFieldValue -> Maybe FITActivityType
parseFitActivity (FitEnumV [a]) = case a of
  0 -> Just FITManual
  1 -> Just FITAutoMultiSport
  _ -> Nothing
parseFitActivity _ = Nothing
