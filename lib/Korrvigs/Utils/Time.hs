module Korrvigs.Utils.Time (addNominal, addCalendar) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

addNominal :: NominalDiffTime -> ZonedTime -> ZonedTime
addNominal diff zt = utcToZonedTime (zonedTimeZone zt) $ addUTCTime diff $ zonedTimeToUTC zt

addCalendar :: CalendarDiffTime -> ZonedTime -> ZonedTime
addCalendar (CalendarDiffTime mths diff) (ZonedTime (LocalTime day time) tz) =
  addNominal diff $ ZonedTime (LocalTime (addMonths (fromInteger mths) day) time) tz

addMonths :: Int -> Day -> Day
addMonths mths day = fromGregorian y' m' d
  where
    (y, m, d) = toGregorian day
    m' = 1 + (m + mths - 1) `mod` 12
    y' = y + toInteger ((m + mths - 1) `div` 12)
