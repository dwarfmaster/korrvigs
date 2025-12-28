module Korrvigs.Web.FullCalendar where

import Control.Lens hiding ((.=))
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Time
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Yesod

data CalendarEvent = CalendarEvent
  { _evTitle :: Text,
    _evStart :: ZonedTime,
    _evEnd :: Maybe ZonedTime,
    _evUrl :: Maybe Text,
    _evAllDay :: Maybe Bool,
    _evColor :: Maybe Text
  }

makeLenses ''CalendarEvent

instance ToJSON CalendarEvent where
  toJSON ev =
    object $
      [ "title" .= (ev ^. evTitle),
        "start" .= (ev ^. evStart)
      ]
        ++ maybe [] (\end -> ["end" .= end]) (ev ^. evEnd)
        ++ maybe [] (\allDay -> ["allDay" .= allDay]) (ev ^. evAllDay)
        ++ maybe [] (\url -> ["url" .= url]) (ev ^. evUrl)
        ++ maybe [] (\col -> ["color" .= col]) (ev ^. evColor)

entryToEvent :: EntryRowR -> Handler (Maybe CalendarEvent)
entryToEvent entry = runMaybeT $ do
  let title = entry ^. sqlEntryTitle
  render <- lift getUrlRender
  public <- lift isPublic
  start <- hoistMaybe $ entry ^. sqlEntryDate
  let dur = fromMaybe noDuration $ entry ^. sqlEntryDuration
  let hasDur = isJust $ entry ^. sqlEntryDuration
  let end = addCalendar dur start
  let allDay = extractTime start == daySep && hasDur && extractTime end == daySep
  pure $
    CalendarEvent
      { _evTitle = fromMaybe ("@" <> unId i) title,
        _evStart = start,
        _evEnd = if hasDur then Just end else Nothing,
        _evAllDay = Just allDay,
        _evUrl = if public then Nothing else Just $ render $ EntryR $ WId i,
        _evColor = Nothing
      }
  where
    i = entry ^. sqlEntryName
    daySep = TimeOfDay 0 0 0
    extractTime = localTimeOfDay . zonedTimeToLocalTime
    noDuration = CalendarDiffTime 0 $ secondsToNominalDiffTime 0

logic :: JavascriptUrl url
logic =
  [julius|
  function setupFullCalendar(calId, events) {
    document.addEventListener('DOMContentLoaded', function() {
      const calendarEl = document.getElementById(calId)
      var calendar = new FullCalendar.Calendar(calendarEl, {
        initialView: 'dayGridMonth',
        events: events,
        initialDate: (events.length > 0) ? events[0].start : null
      });
      calendar.render()
      if(!window.hasOwnProperty("updateFolded")) {
        window.updateFolded = []
      }
      updateFolded.push(function () {
        if(calendarEl.offsetWidth != 0 && calendarEl.offsetHeight != 0) {
          calendar.render();
          return true;
        }
        return false;
      });
    })
  }
|]

header :: Widget
header = do
  Rcs.fullcalendar StaticR
  toWidget logic

widget :: [CalendarEvent] -> Handler Widget
widget events = do
  calId <- newIdent
  pure $ do
    toWidget [julius|setupFullCalendar(#{calId}, #{rawJSON events})|]
    [whamlet|
      <div ##{calId}>
    |]
