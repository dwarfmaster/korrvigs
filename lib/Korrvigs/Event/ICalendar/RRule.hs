module Korrvigs.Event.ICalendar.RRule where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time.Calendar
import Data.Time.Calendar.Month
import Data.Time.LocalTime
import Korrvigs.Event.ICalendar.Defs

data CalendarTime = CalTime
  { _calYear :: Year,
    _calMonth :: MonthOfYear,
    _calDay :: DayOfMonth,
    _calHour :: Int,
    _calMinute :: Int,
    _calSec :: Int
  }
  deriving (Eq, Show)

makeLenses ''CalendarTime

julianDay :: Lens' CalendarTime Day
julianDay =
  lens
    (\time -> fromGregorian (time ^. calYear) (time ^. calMonth) (time ^. calDay))
    (\time day -> let (yr, mt, dy) = toGregorian day in time & calYear .~ yr & calMonth .~ mt & calDay .~ dy)

weekDay :: Getter CalendarTime DayOfWeek
weekDay = julianDay . to dayOfWeek

nextTime :: ICalFreq -> CalendarTime -> CalendarTime
nextTime Secondly time | time ^. calSec < 59 = time & calSec %~ (+ 1)
nextTime Secondly time = nextTime Minutely $ time & calSec .~ 0
nextTime Minutely time | time ^. calMinute < 59 = time & calMinute %~ (+ 1)
nextTime Minutely time = nextTime Hourly $ time & calMinute .~ 0
nextTime Hourly time | time ^. calHour < 23 = time & calHour %~ (+ 1)
nextTime Hourly time = nextTime Daily $ time & calHour .~ 0
nextTime Daily time =
  let year = time ^. calYear
   in let month = time ^. calMonth
       in let day = time ^. calDay
           in let julMonth = YearMonth year month
               in let julDay = YearMonthDay year month day
                   in if periodLastDay julMonth == julDay
                        then nextTime Monthly $ time & calDay .~ 1
                        else time & calDay %~ (+ 1)
nextTime Weekly time = (appEndo $ mconcat $ replicate 7 $ Endo $ nextTime Daily) time
nextTime Monthly time | time ^. calMonth < December = time & calMonth %~ (+ 1)
nextTime Monthly time = nextTime Yearly $ time & calMonth .~ January
nextTime Yearly time = time & calYear %~ (+ 1)

checkBySec :: Int -> CalendarTime -> Bool
checkBySec sec time = time ^. calSec == sec

checkByMin :: Int -> CalendarTime -> Bool
checkByMin mn time = time ^. calMinute == mn

checkByHour :: Int -> CalendarTime -> Bool
checkByHour hour time = time ^. calHour == hour

checkWeekOffset :: forall f p. (DayPeriod p) => f p -> Day -> Int -> Bool
checkWeekOffset _ day offset =
  if offset >= 0
    then doCheck offset $ diffDays day (periodFirstDay period)
    else doCheck (-offset) $ diffDays (periodLastDay period) day
  where
    period :: p
    period = dayPeriod day
    doCheck :: Int -> Integer -> Bool
    doCheck o d = (o - 1) * 7 <= fromInteger d && o * 7 > fromInteger d

checkByDay :: ICalFreq -> (Maybe Int, DayOfWeek) -> CalendarTime -> Bool
checkByDay Monthly (Just offset, day) time =
  time ^. weekDay == day && checkWeekOffset (Nothing :: Maybe Month) (time ^. julianDay) offset
checkByDay Yearly (Just offset, day) time =
  time ^. weekDay == day && checkWeekOffset (Nothing :: Maybe Year) (time ^. julianDay) offset
-- When the frequence is not Monthly or Yearly, ingore offset (it should not be present
-- in valid icalendar data anyway).
checkByDay _ (_, day) time = time ^. weekDay == day

checkDayInPeriod :: forall f p. (DayPeriod p) => f p -> Day -> Int -> Bool
checkDayInPeriod _ day offset
  | offset >= 0 =
      addDays (toInteger $ offset - 1) (periodFirstDay (dayPeriod day :: p)) == day
checkDayInPeriod _ day offset =
  addDays (toInteger $ -offset - 1) day == periodLastDay (dayPeriod day :: p)

checkByMonthDay :: Int -> CalendarTime -> Bool
checkByMonthDay md time = checkDayInPeriod (Nothing :: Maybe Month) (time ^. julianDay) md

checkByYearDay :: Int -> CalendarTime -> Bool
checkByYearDay yd time = checkDayInPeriod (Nothing :: Maybe Year) (time ^. julianDay) yd

checkByWeekNo :: Maybe DayOfWeek -> Int -> CalendarTime -> Bool
checkByWeekNo _ _ _ = error "BYWEEKNO not implemented"

checkByMonth :: Int -> CalendarTime -> Bool
checkByMonth month time = time ^. calMonth == month

timeRange :: (Alternative m) => Int -> ICalFreq -> CalendarTime -> m CalendarTime
timeRange 0 _ _ = empty
timeRange n freq start = pure start <|> timeRange (n - 1) freq (nextTime freq start)

guardRange :: (Monad m, Alternative m) => Bool -> Int -> ICalFreq -> [a] -> (a -> CalendarTime -> Bool) -> CalendarTime -> m CalendarTime
guardRange False _ _ _ _ start = pure start
guardRange _ _ _ [] _ start = pure start
guardRange _ n freq cases check start = do
  time <- timeRange n freq start
  guard $ any (`check` time) cases
  pure time

data BySelector = BySel
  { _bySec :: Bool,
    _byMin :: Bool,
    _byHour :: Bool,
    _byDay :: Bool,
    _byMonthDay :: Bool,
    _byYearDay :: Bool,
    _byWeekNo :: Bool,
    _byMonth :: Bool
  }

makeLenses ''BySelector

-- TODO fix daily logic
applyRRule :: (Monad m, Alternative m) => BySelector -> ICalRRule -> CalendarTime -> m CalendarTime
applyRRule sel rrule time =
  guardRange (sel ^. byMonth) 12 Monthly (rrule ^. icrrByMonth) checkByMonth time
    >>= guardRange (sel ^. byWeekNo) 53 Weekly (rrule ^. icrrByWeekNo) (checkByWeekNo $ rrule ^. icrrWkst)
    >>= guardRange (sel ^. byYearDay) daysToNextYear Daily (rrule ^. icrrByYearDay) checkByYearDay
    >>= guardRange (sel ^. byMonthDay) daysInMonth Daily (rrule ^. icrrByMonthDay) checkByMonthDay
    >>= guardRange (sel ^. byDay) daysInMonth Daily (rrule ^. icrrByDay) (checkByDay Monthly)
    >>= guardRange (sel ^. byHour) 24 Hourly (rrule ^. icrrByHour) checkByHour
    >>= guardRange (sel ^. byMin) 60 Minutely (rrule ^. icrrByMin) checkByMin
    >>= guardRange (sel ^. bySec) 60 Secondly (rrule ^. icrrBySec) checkBySec
  where
    daysToNextYear :: Int
    daysToNextYear =
      if isLeapYear (time ^. calYear) && (time ^. calMonth == January || time ^. calMonth == February && time ^. calDay < 29)
        then 366
        else 365
    daysInMonth :: Int
    daysInMonth = periodLength (dayPeriod (time ^. julianDay) :: Month)

negBySelector :: BySelector -> BySelector
negBySelector =
  foldr
    (.)
    id
    [ bySec %~ not,
      byMin %~ not,
      byHour %~ not,
      byDay %~ not,
      byMonthDay %~ not,
      byYearDay %~ not,
      byWeekNo %~ not,
      byMonth %~ not
    ]

expandSelector :: ICalFreq -> BySelector
expandSelector Yearly = BySel True True True True True True True True
expandSelector Monthly = BySel True True True True True True True False
expandSelector Weekly = BySel True True True True True True True False
expandSelector Daily = BySel True True True False False False False False
expandSelector Hourly = BySel True True False False False False False False
expandSelector Minutely = BySel True False False False False False False False
expandSelector Secondly = BySel False False False False False False False False

limitRRule :: ICalRRule -> CalendarTime -> Maybe CalendarTime
limitRRule rrule time = listToMaybe $ applyRRule sel rrule time
  where
    sel :: BySelector
    sel = negBySelector $ expandSelector $ rrule ^. icrrFreq

rruleInstances :: ICalRRule -> CalendarTime -> [CalendarTime]
rruleInstances rrule = limEnd . limCount . mapMaybe (limitRRule rrule) . unfoldr (Just . dup . nextInstance)
  where
    nextInstance :: CalendarTime -> CalendarTime
    nextInstance = foldr (.) id $ replicate (fromMaybe 1 $ rrule ^. icrrInterval) $ nextTime $ rrule ^. icrrFreq
    dup :: a -> (a, a)
    dup x = (x, x)
    limCount :: [a] -> [a]
    limCount = maybe id take $ rrule ^. icrrCount
    limEnd :: [CalendarTime] -> [CalendarTime]
    limEnd = maybe id (\lt -> takeWhile (\ct -> calendarToLocalTime ct < lt)) $ rrule ^. icrrUntil

expandRRule :: ICalRRule -> CalendarTime -> [CalendarTime]
expandRRule rrule time = applyRRule sel rrule time
  where
    sel :: BySelector
    sel = expandSelector $ rrule ^. icrrFreq

fixStartTime :: ICalRRule -> CalendarTime -> CalendarTime
fixStartTime rrule (CalTime year month day hour mn sec) =
  CalTime
    year
    (if rrule ^. icrrByMonth /= [] then January else month)
    ( if rrule ^. icrrByDay /= [] || rrule ^. icrrByMonthDay /= [] || rrule ^. icrrByYearDay /= []
        then 1
        else day
    )
    (if rrule ^. icrrByHour /= [] then 0 else hour)
    (if rrule ^. icrrByMin /= [] then 0 else mn)
    (if rrule ^. icrrBySec /= [] then 0 else sec)

calendarToLocalTime :: CalendarTime -> LocalTime
calendarToLocalTime cal =
  LocalTime
    (cal ^. julianDay)
    $ TimeOfDay (cal ^. calHour) (cal ^. calMinute) (fromInteger $ toInteger $ cal ^. calSec)

localToCalendarTime :: LocalTime -> CalendarTime
localToCalendarTime (LocalTime day (TimeOfDay hour mn sec)) =
  let cal = CalTime 0 0 0 hour mn (truncate sec)
   in cal & julianDay .~ day

executeRRule :: LocalTime -> ICalRRule -> [LocalTime]
executeRRule time rrule = calendarToLocalTime <$> (rruleInstances rrule start >>= expandRRule rrule)
  where
    start = fixStartTime rrule $ localToCalendarTime time
