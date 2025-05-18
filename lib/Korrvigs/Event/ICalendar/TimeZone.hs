module Korrvigs.Event.ICalendar.TimeZone where

import Control.Lens
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Korrvigs.Event.ICalendar.Defs
import Korrvigs.Event.ICalendar.RRule

timeZoneTimes :: ICalTimeZone -> [(LocalTime, ICalTZSpec)]
timeZoneTimes tz = foldr (merge . mkSpec) [] $ tz ^. ictzSpecs
  where
    mkSpec :: ICalTZSpec -> [(LocalTime, ICalTZSpec)]
    mkSpec spec =
      (,spec) <$> maybe [spec ^. ictzStart] (executeRRule (spec ^. ictzStart)) (spec ^. ictzRRule)
    merge :: [(LocalTime, ICalTZSpec)] -> [(LocalTime, ICalTZSpec)] -> [(LocalTime, ICalTZSpec)]
    merge [] ys = ys
    merge xs [] = xs
    merge (x : xs) (y : ys) | x ^. _1 < y ^. _1 = x : merge xs (y : ys)
    merge (x : xs) (y : ys) = y : merge (x : xs) ys

solveTimeZone :: LocalTime -> [(LocalTime, ICalTZSpec)] -> ICalTZSpec
solveTimeZone _ [] = ICTZSpec True (LocalTime (fromOrdinalDate 1970 1) (TimeOfDay 0 0 0)) 0 0 Nothing (Just "utc") Nothing def
solveTimeZone time (tz : _) | time > tz ^. _1 = tz ^. _2
solveTimeZone time (_ : tzs) = solveTimeZone time tzs

-- Use the timezones to find the zone of a time, and use it to construct a zoned time
resolveICalTime :: ICalFile -> ICalTimeSpec -> ZonedTime
resolveICalTime file time =
  if time ^. ictmUTC
    then ZonedTime (time ^. ictmDate) utc
    else case time ^. ictmTimeZone >>= flip M.lookup (file ^. icTimezones) of
      Nothing -> ZonedTime (time ^. ictmDate) utc
      Just tz ->
        let spec = solveTimeZone (time ^. ictmDate) $ timeZoneTimes tz
         in ZonedTime (time ^. ictmDate) $ TimeZone (spec ^. ictzOffsetTo `div` 60) (not $ spec ^. ictzStandard) (maybe "" T.unpack $ spec ^. ictzName)

-- Find a timezone in the ical file that resolves to the right timezone
findTimeZone :: ZonedTime -> ICalFile -> Maybe Text
findTimeZone time file = M.lookup (timeZoneMinutes $ zonedTimeZone time) resolved
  where
    dummySpec :: Text -> ICalTimeSpec
    dummySpec tz = ICTmSpec (zonedTimeToLocalTime time) False (Just tz)
    resolved :: Map Int Text
    resolved = M.fromList $ fmap (\(tz, _) -> (,tz) $ timeZoneMinutes $ zonedTimeZone $ resolveICalTime file (dummySpec tz)) $ M.toList $ file ^. icTimezones

-- Create a time spec for a zoned time, creating a timezone if necessary
timeToICalSpec :: ZonedTime -> ICalFile -> (ICalTimeSpec, ICalFile)
timeToICalSpec time file = case findTimeZone time file of
  Just tzname -> (ICTmSpec local False (Just tzname), file)
  Nothing ->
    let spec = ICTmSpec local False (Just name)
     in let year :: Year = dayPeriod (localDay local)
         in let tzstart = LocalTime (fromOrdinalDate (min 1970 year) 1) (TimeOfDay 0 0 0)
             in let tzspec = ICTZSpec True tzstart 0 (timeZoneMinutes tz) Nothing Nothing Nothing def
                 in let ntz = ICTZ name [tzspec] def
                     in let nfile = file & icTimezones %~ M.insert name ntz
                         in (spec, nfile)
  where
    tz :: TimeZone
    tz = zonedTimeZone time
    local :: LocalTime
    local = zonedTimeToLocalTime time
    stub :: Text
    stub = if timeZoneName tz == "" then "tz" else T.pack $ timeZoneName tz
    name :: Text
    name =
      let names =
            dropWhile (flip M.member $ file ^. icTimezones) $
              (\(n :: Int) -> stub <> T.pack (show n)) <$> [1 ..]
       in case names of
            [] -> error "Impossible"
            (h : _) -> h
