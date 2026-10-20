module Korrvigs.Utils.Time
  ( fromGreg,
    addNominal,
    addCalendar,
    dayToZonedTime,
    zonedDay,
    getCurrentZonedTime,
    isValidDate,
    measureTime,
    measureTimeMs,
    measureTime_,
  )
where

import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.LocalTime
import System.Clock

fromGreg :: (Integral yr) => yr -> Maybe MonthOfYear -> Maybe DayOfMonth -> Day
fromGreg year month day = fromGregorian (toInteger year) (fromMaybe 1 month) (fromMaybe 1 day)

dayToZonedTime :: TimeZone -> Day -> ZonedTime
dayToZonedTime tz day = ZonedTime (LocalTime day (TimeOfDay 0 0 0)) tz

zonedDay :: ZonedTime -> Day
zonedDay = localDay . zonedTimeToLocalTime

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

getCurrentZonedTime :: IO ZonedTime
getCurrentZonedTime = do
  tz <- getCurrentTimeZone
  local <- utcToLocalTime tz <$> getCurrentTime
  pure $ ZonedTime local tz

isValidDate :: Maybe ZonedTime -> Bool
isValidDate dt =
  (dt ^? _Just . to zonedTimeToLocalTime . to localDay . to toOrdinalDate . _1) /= Just 0

measureTime :: (MonadIO m) => m a -> m (Text, a)
measureTime = measureTimeImpl printTime
  where
    printTime :: TimeSpec -> Text
    printTime (TimeSpec s ns) | s /= 0 || ns > 1000000 = T.pack (show $ s * 1000 + ns `div` 1000000) <> "ms"
    printTime (TimeSpec _ ns) | ns > 1000 = T.pack (show $ ns `div` 1000) <> "μs"
    printTime (TimeSpec _ ns) = T.pack (show ns) <> "ns"

measureTimeMs :: (MonadIO m) => m a -> m (Int, a)
measureTimeMs = measureTimeImpl toMs
  where
    toMs (TimeSpec s ns) = fromInteger $ toInteger $ s * 1000 + ns `div` 1000000

measureTimeImpl :: (MonadIO m) => (TimeSpec -> r) -> m a -> m (r, a)
measureTimeImpl f act = do
  tm1 <- liftIO $ getTime Monotonic
  r <- act
  tm2 <- liftIO $ getTime Monotonic
  pure (f $ tm2 - tm1, r)

measureTime_ :: (MonadIO m) => m a -> m Text
measureTime_ = fmap fst . measureTime
