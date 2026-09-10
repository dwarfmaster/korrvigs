module Korrvigs.Web.Widgets.TimeEvent (timeEventsWidget) where

import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Contact
import Korrvigs.Monad
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Opaleye hiding (not, null)
import Yesod hiding (Attr, Field)

data TimeEventKind
  = Anniversary
  | ScheduledTask
  | DeadlineTask
  | StartedTask

-- The two times must be less than one year appart
birthdayTimeEvents :: Day -> Day -> Handler [(TimeEventKind, Day, Widget)]
birthdayTimeEvents startDay endDay = do
  birthers <- rSelect $ do
    bdaymtdt <- selectTable entriesMetadataTable
    where_ $ bdaymtdt ^. sqlKey .== sqlStrictText (mtdtSqlName BirthDayMtdt)
    where_ $
      matchNullable (sqlBool False) cond $
        sqlJsonToText $
          toNullable $
            bdaymtdt ^. sqlValue
    bdayYear <- sqlJsonToNum <$> selectMtdt BirthYear (bdaymtdt ^. sqlEntry)
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryId .== bdaymtdt ^. sqlEntry
    pure (entry ^. sqlEntryName, entry ^. sqlEntryTitle, (bdayYear, bdaymtdt ^. sqlValue))
  let birthersPrepared = mapMaybe prepBirther birthers
  forM birthersPrepared $ \(nm, title, (byear, bday)) -> do
    let widget =
          [whamlet|
            <a href=@{EntryR (WId nm)}>
              #{title}
            #{mconcat [" (", T.pack (show $ ageAt byear bday), " years old on ", rdrBday bday, ")"]}
         |]
    let BirthDay bdayMonth bdayDay = bday
    let actualDay = fromGregorian (birthYear bday) bdayMonth bdayDay
    pure (Anniversary, actualDay, widget)
  where
    prepBirther :: (Id, Maybe Text, (Maybe Double, Value)) -> Maybe (Id, Text, (Double, BirthDay))
    prepBirther (nm, Just title, (Just yr, bdayJS)) =
      (nm,title,) . (yr,) <$> fromJSONM bdayJS
    prepBirther _ = Nothing
    (year, month, dnum) = toGregorian startDay
    (endYear, endMonth, endDnum) = toGregorian endDay
    startBirth = renderBirthday $ BirthDay month dnum
    endBirth = renderBirthday $ BirthDay endMonth endDnum
    birthOp = if month == December then (.||) else (.&&)
    cond bday = (bday .>= sqlStrictText startBirth) `birthOp` (bday .<= sqlStrictText endBirth)
    birthYear :: BirthDay -> Year
    birthYear (BirthDay mth day) =
      if mth < month || (mth == month && day < dnum) then endYear else year
    rdrBday :: BirthDay -> Text
    rdrBday bday@(BirthDay mth day) =
      T.pack $ formatTime defaultTimeLocale "%A %d, %B" $ fromGregorian (birthYear bday) mth day
    ageAt :: Double -> BirthDay -> Integer
    ageAt byear bday@(BirthDay mth day) =
      snd $ computeAgeAt (floor byear) (Just bday) $ fromGregorian (birthYear bday) mth day

timeWidget :: [(TimeEventKind, Day, Widget)] -> Handler Widget
timeWidget timeEvents = do
  currentDay <- utctDay <$> liftIO getCurrentTime
  let eventsWidgets = map (prepEvent currentDay) timeEventsSorted
  pure
    [whamlet|
      <ul>
        $forall widget <- eventsWidgets
          <li>
            ^{widget}
    |]
  where
    timeEventsSorted = sortBy (\t1 t2 -> compare (t1 ^. _2) (t2 ^. _2)) timeEvents
    prepEvent currentDay (kd, day, widget) =
      let dist = diffDays day currentDay
       in let printedDist = if dist == 0 then "Today" else if dist > 0 then "In " <> T.pack (show dist) <> " days" else T.pack (show $ -dist) <> " days ago"
           in [whamlet|
        #{mconcat [kdWidget kd, " ", printedDist]},
        ^{widget}
      |]
    kdWidget :: TimeEventKind -> Text
    kdWidget Anniversary = "🎉"
    kdWidget ScheduledTask = "📅"
    kdWidget DeadlineTask = "🛑"
    kdWidget StartedTask = "🕒"

-- The two times must be less than one year appart
timeEventsWidget :: Day -> Day -> Handler (Maybe Widget)
timeEventsWidget startDay endDay = do
  bdays <- birthdayTimeEvents startDay endDay
  let allEvents = mconcat [bdays]
  case allEvents of
    [] -> pure Nothing
    _ -> Just <$> timeWidget allEvents
