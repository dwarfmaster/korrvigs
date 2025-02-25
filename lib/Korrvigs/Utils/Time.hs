module Korrvigs.Utils.Time
  ( addNominal,
    addCalendar,
    dayToZonedTime,
    measureTime,
    measureTime_,
  )
where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import System.Clock

dayToZonedTime :: TimeZone -> Day -> ZonedTime
dayToZonedTime tz day = ZonedTime (LocalTime day (TimeOfDay 0 0 0)) tz

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

measureTime :: (MonadIO m) => m a -> m (Text, a)
measureTime act = do
  tm1 <- liftIO $ getTime Monotonic
  r <- act
  tm2 <- liftIO $ getTime Monotonic
  pure (printTime $ tm2 - tm1, r)
  where
    printTime :: TimeSpec -> Text
    printTime (TimeSpec s ns) | s /= 0 || ns > 1000000 = T.pack (show $ s * 1000 + ns `div` 1000000) <> "ms"
    printTime (TimeSpec _ ns) | ns > 1000 = T.pack (show $ ns `div` 1000) <> "μs"
    printTime (TimeSpec _ ns) = T.pack (show ns) <> "ns"

measureTime_ :: (MonadIO m) => m a -> m Text
measureTime_ = fmap fst . measureTime
