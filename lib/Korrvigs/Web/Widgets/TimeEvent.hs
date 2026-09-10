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
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Note.Loc
import Korrvigs.Note.SQL
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye
import Korrvigs.Utils.Time
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Wdgs
import Opaleye hiding (not, null)
import Yesod hiding (Attr, Field)

data TimeEventKind
  = Anniversary
  | ScheduledTask
  | DeadlineTask

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

deadlineEvents :: Day -> Day -> Handler [(TimeEventKind, Day, Widget)]
deadlineEvents _ endDay = do
  tz <- liftIO getCurrentTimeZone
  -- let startTime = ZonedTime (LocalTime startDay (TimeOfDay 0 0 0)) tz
  let endTime = ZonedTime (LocalTime (addDays 1 endDay) (TimeOfDay 0 0 0)) tz
  evs <- rSelect $ do
    tsk <- selectTaskRows
    deadline <- fromNullableSelect $ pure $ tsk ^. noteTaskDeadline
    where_ $ deadline .<= sqlZonedTime endTime
    where_ $ tsk ^. noteTaskStatus ./= sqlStrictText (renderTaskStatus TaskDone)
    where_ $ tsk ^. noteTaskStatus ./= sqlStrictText (renderTaskStatus TaskDont)
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryId .== tsk ^. noteTaskNote
    pure (entry ^. sqlEntryName, tsk ^. noteTaskTitle, tsk ^. noteTaskRef, tsk ^. noteTaskStatus, deadline)
  forM evs $ \(i, title :: Text, ref, status, deadline) -> do
    render <- getUrlRenderParams
    let actualStatus = fromMaybe TaskTodo $ parseStatusName status
    let (openUrl, postUrl) = renderUrl render i ref
    (h, cbw, _) <- Wdgs.checkBox actualStatus postUrl
    let checkbox = cbw >> toWidget h
    let widget =
          [whamlet|
           ^{checkbox}
           <a href=#{openUrl}>
             #{title}
           #{mconcat ["before ", renderTime deadline]}
         |]
    pure (DeadlineTask, zonedDay deadline, widget)
  where
    renderTime :: ZonedTime -> Text
    renderTime = T.pack . formatTime defaultTimeLocale "%R, %A %d, %B"
    renderUrl :: (Route WebData -> [(Text, Text)] -> Text) -> Id -> Maybe Text -> (Text, Route WebData)
    renderUrl render i sb =
      case parseLoc <$> sb of
        Just (Right (LocSub loc)) ->
          let openUrl = render (EntryR $ WId i) [("open", renderEmbeddedLoc (DeepEmbedLoc [], loc))]
           in (openUrl, NoteSubR (WId i) (WLoc $ LocTask $ TaskLoc loc))
        _ ->
          (render (EntryR $ WId i) [], EntryMtdtR $ WId i)

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

-- The two times must be less than one year appart
timeEventsWidget :: Day -> Day -> Handler (Maybe Widget)
timeEventsWidget startDay endDay = do
  bdays <- birthdayTimeEvents startDay endDay
  deadlines <- deadlineEvents startDay endDay
  let allEvents = mconcat [bdays, deadlines]
  case allEvents of
    [] -> pure Nothing
    _ -> Just <$> timeWidget allEvents
