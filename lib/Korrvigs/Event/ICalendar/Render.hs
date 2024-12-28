module Korrvigs.Event.ICalendar.Render where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as B8
import Data.Fixed
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Korrvigs.Event.ICalendar.Defs
import Network.URI
import Numeric (showEFloat)

renderICalFile :: ICalFile -> BSL.ByteString
renderICalFile = toLazyByteString . buildICalFile

buildICalFile :: ICalFile -> Builder
buildICalFile = execWriter . flip evalStateT 0 . bldFile

type RenderM = StateT Int (Writer Builder)

bldChar :: Char -> RenderM ()
bldChar c = do
  pos <- get
  let utf8 = B8.fromChar c
  let l = BS.length utf8
  when (pos + l > 75) $ do
    tell $ word8 0x0A
    tell $ word8 0x20
    put 1
  tell $ byteString utf8
  modify (+ l)

bldNewline :: RenderM ()
bldNewline = tell (word8 0x0A) >> put 0

bldText :: Text -> RenderM ()
bldText = mapM_ bldChar . T.unpack

bldSepBy :: RenderM () -> [RenderM ()] -> RenderM ()
bldSepBy sep vals = sequence_ $ intersperse sep vals

shouldQuote :: Text -> Bool
shouldQuote = T.any (`elem` [':', ';', ','])

bldParamValue :: Text -> RenderM ()
bldParamValue v | shouldQuote v = bldChar '"' >> bldText v >> bldChar '"'
bldParamValue v = bldText v

bldLine :: (a -> RenderM ()) -> Text -> ICalValue a -> RenderM ()
bldLine rdr name val = do
  bldText name
  forM_ (M.toList $ val ^. icParams) $ \(param, pvalue) -> do
    bldChar ';'
    bldText param
    bldChar '='
    bldSepBy (bldChar ',') $ bldParamValue <$> pvalue
  bldChar ':'
  rdr $ val ^. icValue
  bldNewline

bldLineDef :: Text -> ICalValue Text -> RenderM ()
bldLineDef = bldLine bldText

runProduct :: (Monad m) => (a -> b -> m ()) -> (a, [b]) -> m ()
runProduct f (x, l) = mapM_ (uncurry f) $ (x,) <$> l

bldAbstractGroup :: ICalAbstractGroup -> RenderM ()
bldAbstractGroup group = do
  mapM_ (runProduct bldLineDef) $ M.toList $ group ^. icValues
  mapM_ (runProduct bldGroup) $ M.toList $ group ^. icGroups

bldGroup :: Text -> ICalAbstractGroup -> RenderM ()
bldGroup tp group = do
  bldLineDef "BEGIN" $ ICValue M.empty tp
  bldAbstractGroup group
  bldLineDef "END" $ ICValue M.empty tp

bldBinary :: BS.ByteString -> RenderM ()
bldBinary = bldText . B64.encodeBase64

bldBool :: Bool -> RenderM ()
bldBool True = bldText "TRUE"
bldBool False = bldText "FALSE"

bldDigits :: (Num a, Show a) => Int -> a -> RenderM ()
bldDigits n v = bldText $ T.pack s'
  where
    s = show v
    l = length s
    s' = if l >= n then drop (l - n) s else replicate (n - l) '0' ++ s

bldNumber :: (Num a, Show a) => a -> RenderM ()
bldNumber = bldText . T.pack . show

bldDate :: Day -> RenderM ()
bldDate d = do
  let (yr, mth, day) = toGregorian d
  bldDigits 4 yr
  bldDigits 2 mth
  bldDigits 2 day

bldTime :: Bool -> TimeOfDay -> RenderM ()
bldTime isUTC (TimeOfDay hr mn sec) = do
  bldDigits 2 hr
  bldDigits 2 mn
  bldDigits 2 sec
  when isUTC $ bldText "Z"

bldDateTime :: Bool -> LocalTime -> RenderM ()
bldDateTime isUTC (LocalTime date time) = do
  bldDate date
  bldText "T"
  bldTime isUTC time

bldSign :: Bool -> RenderM ()
bldSign True = bldText "+"
bldSign False = bldText "-"

bldDuration :: NominalDiffTime -> RenderM ()
bldDuration diff = do
  bldSign $ picos >= 0
  bldText "P"
  if isWeeks
    then do
      bldNumber $ seconds `div` weekSecs
      bldText "W"
    else do
      let days = seconds `div` daySecs
      let hours = (seconds `mod` daySecs) `div` hourSecs
      let minutes = (seconds `mod` hourSecs) `div` minuteSecs
      let final = seconds `mod` minuteSecs
      when (days > 0) $ bldNumber days >> bldText "D"
      when (seconds `mod` daySecs > 0) $ bldText "T"
      when (hours > 0) $ bldNumber hours >> bldText "H"
      when (minutes > 0) $ bldNumber minutes >> bldText "M"
      when (final > 0) $ bldNumber final >> bldText "S"
  where
    MkFixed picos = nominalDiffTimeToSeconds diff
    seconds = abs $ picos `div` resolution (Nothing :: Maybe E12)
    minuteSecs = 60
    hourSecs = 60 * minuteSecs
    daySecs = 24 * hourSecs
    weekSecs = 7 * daySecs
    isWeeks = seconds `mod` weekSecs == 0

bldFloat :: Double -> RenderM ()
bldFloat = bldText . T.pack . flip (showEFloat Nothing) ""

bldInteger :: Int -> RenderM ()
bldInteger n = do
  bldSign $ n >= 0
  bldNumber $ abs n

bldPeriod :: LocalTime -> LocalTime -> RenderM ()
bldPeriod start end = do
  bldDateTime False start
  bldText "/"
  bldDateTime False end

bldTextValue :: Text -> RenderM ()
bldTextValue = bldText . T.concatMap escape
  where
    escape :: Char -> Text
    escape '\n' = "\\n"
    escape ',' = "\\,"
    escape ';' = "\\;"
    escape '\\' = "\\\\"
    escape c = T.singleton c

bldUri :: URI -> RenderM ()
bldUri = bldText . T.pack . show

bldUtcOffset :: Int -> RenderM ()
bldUtcOffset offset = do
  bldSign $ offset >= 0
  let hours = offset `div` 3600
  let minutes = (offset `mod` 3600) `div` 60
  let seconds = offset `mod` 60
  bldDigits 2 hours
  bldDigits 2 minutes
  when (seconds > 0) $ bldDigits 2 seconds

bldFreq :: ICalFreq -> RenderM ()
bldFreq Secondly = bldText "SECONDLY"
bldFreq Minutely = bldText "MINUTELY"
bldFreq Hourly = bldText "HOURLY"
bldFreq Daily = bldText "DAILY"
bldFreq Weekly = bldText "WEEKLY"
bldFreq Monthly = bldText "MONTHLY"
bldFreq Yearly = bldText "YEARLY"

bldWeekday :: DayOfWeek -> RenderM ()
bldWeekday Sunday = bldText "SU"
bldWeekday Monday = bldText "MO"
bldWeekday Tuesday = bldText "TU"
bldWeekday Wednesday = bldText "WE"
bldWeekday Thursday = bldText "TH"
bldWeekday Friday = bldText "FR"
bldWeekday Saturday = bldText "SA"

bldSignedNumber :: (Num a, Show a, Ord a) => a -> RenderM ()
bldSignedNumber n = do
  bldSign $ n > 0
  bldText . T.pack . show $ abs n

bldWeekdayNum :: (Maybe Int, DayOfWeek) -> RenderM ()
bldWeekdayNum (offset, day) = do
  maybe (pure ()) bldSignedNumber offset
  bldWeekday day

bldRRule :: ICalRRule -> RenderM ()
bldRRule rrule =
  sequence_ $
    intersperse (bldText ";") $
      catMaybes
        [ Just (bldText "FREQ=" >> bldFreq (rrule ^. icrrFreq)),
          (\u -> bldText "UNTIL=" >> bldDateTime False u) <$> rrule ^. icrrUntil,
          (\c -> bldText "COUNT=" >> bldNumber c) <$> rrule ^. icrrCount,
          (\i -> bldText "INTERVAL=" >> bldNumber i) <$> rrule ^. icrrInterval,
          listOf "BYSECOND=" bldNumber (rrule ^. icrrBySec),
          listOf "BYMINUTE=" bldNumber (rrule ^. icrrByMin),
          listOf "BYHOUR=" bldNumber (rrule ^. icrrByHour),
          listOf "BYDAY=" bldWeekdayNum (rrule ^. icrrByDay),
          listOf "BYMONTHDAY=" bldSignedNumber (rrule ^. icrrByMonthDay),
          listOf "BYYEARDAY=" bldSignedNumber (rrule ^. icrrByYearDay),
          listOf "BYWEEKNO=" bldSignedNumber (rrule ^. icrrByWeekNo),
          listOf "BYMONTH=" bldNumber (rrule ^. icrrByMonth),
          listOf "BYSETPOS=" bldSignedNumber (rrule ^. icrrBySetPos),
          (\w -> bldText "WKST=" >> bldWeekday w) <$> rrule ^. icrrWkst
        ]
  where
    listOf :: Text -> (a -> RenderM ()) -> [a] -> Maybe (RenderM ())
    listOf _ _ [] = Nothing
    listOf prefix rdr xs = Just $ do
      bldText prefix
      sequence_ (intersperse (bldText ",") $ rdr <$> xs)

bldTZSpec :: ICalTZSpec -> RenderM ()
bldTZSpec spec = do
  let gname = if spec ^. ictzStandard then "STANDARD" else "DAYLIGHT"
  bldLineDef "BEGIN" $ ICValue M.empty gname
  bldLine (bldDateTime False) "DTSTART" $ ICValue M.empty $ spec ^. ictzStart
  bldLine bldUtcOffset "TZOFFSETTO" $ ICValue M.empty $ spec ^. ictzOffsetTo
  bldLine bldUtcOffset "TZOFFSETFROM" $ ICValue M.empty $ spec ^. ictzOffsetFrom
  forM_ (spec ^. ictzRdate) $ \dt -> bldLine (bldDateTime False) "RDATE" $ ICValue M.empty dt
  forM_ (spec ^. ictzName) $ \nm -> bldLineDef "TZNAME" $ ICValue M.empty nm
  forM_ (spec ^. ictzRRule) $ \rr -> bldLine bldRRule "RRULE" $ ICValue M.empty rr
  bldAbstractGroup $ spec ^. ictzContent
  bldLineDef "END" $ ICValue M.empty gname

bldTZ :: ICalTimeZone -> RenderM ()
bldTZ tz = do
  bldLineDef "BEGIN" $ ICValue M.empty "VTIMEZONE"
  bldLineDef "TZID" $ ICValue M.empty $ tz ^. ictzId
  mapM_ bldTZSpec $ tz ^. ictzSpecs
  bldAbstractGroup $ tz ^. ictzTopLevel
  bldLineDef "END" $ ICValue M.empty "VTIMEZONE"

bldTimeSpec :: Text -> ICalTimeSpec -> RenderM ()
bldTimeSpec lbl spec =
  bldLine (bldDateTime $ spec ^. ictmUTC) lbl $ ICValue params $ spec ^. ictmDate
  where
    params = case spec ^. ictmTimeZone of
      Just tz -> M.singleton "TZID" [tz]
      Nothing -> M.empty

bldGeo :: (Double, Double) -> RenderM ()
bldGeo (lat, lon) = bldFloat lat >> bldChar ';' >> bldFloat lon

bldTransp :: Bool -> RenderM ()
bldTransp True = bldText "TRANSPARENT"
bldTransp False = bldText "OPAQUE"

bldCategories :: [Text] -> RenderM ()
bldCategories cats = bldSepBy (bldChar ',') $ bldTextValue <$> cats

bldEvent :: ICalEvent -> RenderM ()
bldEvent ev = do
  bldLineDef "BEGIN" $ ic "VEVENT"
  bldLineDef "UID" $ ic $ ev ^. iceUid
  forM_ (ev ^. iceStart) $ bldTimeSpec "DTSTART"
  forM_ (ev ^. iceEnd) $ bldTimeSpec "DTEND"
  forM_ (ev ^. iceDuration) $ bldLine bldDuration "DURATION" . ic
  forM_ (ev ^. iceComment) $ bldLine bldTextValue "COMMENT" . ic
  forM_ (ev ^. iceSummary) $ bldLine bldTextValue "SUMMARY" . ic
  forM_ (ev ^. iceDescription) $ bldLine bldTextValue "DESCRIPTION" . ic
  forM_ (ev ^. iceGeo) $ bldLine bldGeo "GEO" . ic
  forM_ (ev ^. iceLocation) $ bldLine bldTextValue "LOCATION" . ic
  let cats = ev ^. iceCategories
  unless (null cats) $ bldLine bldCategories "CATEGORIES" $ ic cats
  bldLine bldTransp "TRANSP" $ ic $ ev ^. iceTransparent
  bldAbstractGroup $ ev ^. iceContent
  bldLineDef "END" $ ic "VEVENT"
  where
    ic = ICValue M.empty

bldFile :: ICalFile -> RenderM ()
bldFile ical = do
  bldLineDef "BEGIN" $ ICValue M.empty "VCALENDAR"
  bldLineDef "VERSION" $ ICValue M.empty $ ical ^. icVersion
  bldAbstractGroup $ ical ^. icContent
  mapM_ bldEvent $ ical ^. icEvent
  mapM_ bldTZ $ ical ^. icTimezones
  bldLineDef "END" $ ICValue M.empty "VCALENDAR"
