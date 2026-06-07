module Korrvigs.Web.Date (getDateByDayR, getDateByWeekR) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Korrvigs.Monad.Collections
import Korrvigs.Note.AST
import Korrvigs.Query
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Home as Home
import qualified Korrvigs.Web.JS.FullCalendar as FC
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Search.Results
import qualified Korrvigs.Web.Widgets as Widgets
import Text.Blaze
import Yesod

periodPage :: Text -> FC.CalendarEvent -> ZonedTime -> ZonedTime -> Handler Html
periodPage title marker startTime endTime = do
  -- Calendar
  evs <- Home.getEvents startTime
  evsEntries <- mapM FC.entryToEvent evs
  calendar <- FC.widget $ marker : catMaybes evsEntries
  let eventsHd = [whamlet|<h2> ^{Widgets.headerSymbol "🕑"} Calendar|]
  -- All entries
  entries <- runQuery ColList query
  entriesW <- displayResults ColList False entries
  let entriesHd = [whamlet|<h2> ^{Widgets.headerSymbol "✸"} Entries|]
  -- Gallery
  photos <- runQuery ColGallery query
  gallery <- displayResults ColGallery False photos
  let galleryHd = [whamlet|<h2> ^{Widgets.headerSymbol "✿"} Gallery|]
  -- Map
  geo <- runQuery ColMap query
  geoW <- displayResults ColMap False geo
  let geoHd = [whamlet|<h2> ^{Widgets.headerSymbol "○"} Map|]
  -- Building the page
  defaultLayout $ do
    Rcs.entryStyle
    Widgets.sectionLogic
    setTitle $ "Period " <> toMarkup title
    [whamlet|<h1>#{title}|]
    FC.header
    void $ Widgets.mkSection 1 [] [] eventsHd calendar
    let cls = [("class", "collapsed")]
    unless (null entries) $ void $ Widgets.mkSection 1 cls [] entriesHd entriesW
    unless (null photos) $ void $ Widgets.mkSection 1 cls [] galleryHd gallery
    unless (null geo) $ void $ Widgets.mkSection 1 cls [] geoHd geoW
  where
    query :: Query
    query =
      def
        & queryAfter ?~ startTime
        & queryBefore ?~ endTime
        & querySort .~ (ByDate, SortAsc)

getDateByDayR :: Day -> Handler Html
getDateByDayR day = do
  tz <- liftIO getCurrentTimeZone
  let startTime = ZonedTime (LocalTime day time) tz
  let nextDay = addGregorianDurationClip calendarDay day
  let endTime = ZonedTime (LocalTime nextDay time) tz
  let title = formatTime defaultTimeLocale "%A %e, %B %Y" day
  let today = FC.CalendarEvent "Day" startTime Nothing Nothing (Just True) (Just "var(--base0E)")
  periodPage (T.pack title) today startTime endTime
  where
    time = TimeOfDay 0 0 0

getDateByWeekR :: Year -> WeekOfYear -> Handler Html
getDateByWeekR yr wk = do
  tz <- liftIO getCurrentTimeZone
  let startDay = fromMondayStartWeek yr wk 1
  let endDay = addGregorianDurationClip calendarDay $ fromMondayStartWeek yr wk 7
  let startTime = ZonedTime (LocalTime startDay time) tz
  let endTime = ZonedTime (LocalTime endDay time) tz
  let title = formatTime defaultTimeLocale "Week %W, %Y" startDay
  let marker = FC.CalendarEvent "Week" startTime (Just endTime) Nothing (Just True) (Just "var(--base0E)")
  periodPage (T.pack title) marker startTime endTime
  where
    time = TimeOfDay 0 0 0
