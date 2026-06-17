module Korrvigs.Web.Date (getDateByDayR, getDateByWeekR) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Maybe
import Data.Profunctor.Product
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Note.AST
import Korrvigs.Query
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Home as Home
import qualified Korrvigs.Web.JS.FullCalendar as FC
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Search.Results
import qualified Korrvigs.Web.Widgets as Widgets
import Opaleye hiding (not, null)
import Text.Blaze
import Text.Printf
import Yesod
import Prelude hiding (sum)

activityData :: Query -> Handler [(Activity, Double, CalendarDiffTime)]
activityData q = do
  dat <- rSelect $ orderBy (asc $ view _1) $ aggregate (p3 (groupBy, sum, sum)) $ do
    entry <- compileQuery q
    let sqlI = entry ^. sqlEntryId
    activity <- baseSelectTextMtdt ActivityMtdt sqlI
    distanceJS <- selectMtdt DistanceMtdt sqlI
    let distance = fromNullable (sqlDouble 0) $ sqlJsonToNum distanceJS
    let dur = fromNullable (sqlInterval mempty) $ entry ^. sqlEntryDuration
    pure (activity, distance, dur)
  pure $ mapMaybe parseDat dat
  where
    parseDat :: (Text, Double, CalendarDiffTime) -> Maybe (Activity, Double, CalendarDiffTime)
    parseDat (act, dist, dur) = (,dist,dur) <$> parseActivity act

periodPage :: Text -> FC.CalendarEvent -> [(Text, Route WebData)] -> ZonedTime -> ZonedTime -> Handler Html
periodPage title marker linkedDays startTime endTime = do
  -- Linked days
  let linked =
        [whamlet|
    <p>
      $forall (txt,rt) <- linkedDays
        <a href=@{rt}>
          #{txt}
          &nbsp
  |]
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
  activities <- activityData query
  geo <- runQuery ColMap query
  geoW <- displayResults ColMap False geo
  let geoHd = [whamlet|<h2> ^{Widgets.headerSymbol "○"} Map|]
  -- Building the page
  defaultLayout $ do
    Rcs.entryStyle CssR
    Widgets.sectionLogic
    setTitle $ "Period " <> toMarkup title
    [whamlet|<h1>#{title}|]
    FC.header
    linked
    void $ Widgets.mkSection 1 [] [] eventsHd calendar
    let cls = [("class", "collapsed")]
    unless (null entries) $ void $ Widgets.mkSection 1 cls [] entriesHd entriesW
    unless (null photos) $ void $ Widgets.mkSection 1 cls [] galleryHd gallery
    unless (null geo) $ void $ Widgets.mkSection 1 cls [] geoHd $ do
      [whamlet|
        <ul>
          $forall (act,dist,dur) <- activities
            <li>
              #{activityName act}: #{renderDistance dist}, #{renderDuration dur}
      |]
      geoW
  where
    query :: Query
    query =
      def
        & queryAfter ?~ startTime
        & queryBefore ?~ endTime
        & querySort .~ (ByDate, SortAsc)
    renderDistance :: Double -> Text
    renderDistance d | d < 1000 = (T.pack $ show (floor d :: Int)) <> "m"
    renderDistance d = T.pack $ printf "%.3fkm" (d / 1000)
    renderDuration :: CalendarDiffTime -> Text
    renderDuration = T.pack . formatTime defaultTimeLocale "%hh%Mm"

getDateByDayR :: Day -> Handler Html
getDateByDayR day = do
  tz <- liftIO getCurrentTimeZone
  let startTime = ZonedTime (LocalTime day time) tz
  let endTime = ZonedTime (LocalTime nextDay time) tz
  let title = formatTime defaultTimeLocale "%A %e, %B %Y" day
  let today = FC.CalendarEvent "Day" startTime Nothing Nothing (Just True) (Just "var(--base0E)")
  periodPage (T.pack title) today linked startTime endTime
  where
    time = TimeOfDay 0 0 0
    inv = scaleCalendarDiffDays (-1)
    nextDay = addGregorianDurationClip calendarDay day
    nextYear = addGregorianDurationClip calendarYear day
    lastYear = addGregorianDurationClip (inv calendarYear) day
    prevDay = addGregorianDurationClip (inv calendarDay) day
    (wk, _) = mondayStartWeek day
    linked =
      [ ("prev day", DateByDayR prevDay),
        ("last year", DateByDayR lastYear),
        ("this week", DateByWeekR (dayPeriod day) wk),
        ("next year", DateByDayR nextYear),
        ("next day", DateByDayR nextDay)
      ]

getDateByWeekR :: Year -> WeekOfYear -> Handler Html
getDateByWeekR yr wk = do
  tz <- liftIO getCurrentTimeZone
  let startTime = ZonedTime (LocalTime startDay time) tz
  let endTime = ZonedTime (LocalTime endDay time) tz
  let title = formatTime defaultTimeLocale "Week %W, %Y" startDay
  let marker = FC.CalendarEvent "Week" startTime (Just endTime) Nothing (Just True) (Just "var(--base0E)")
  periodPage (T.pack title) marker linked startTime endTime
  where
    time = TimeOfDay 0 0 0
    prevWeek = if wk <= 1 then (yr - 1, 53 + wk - 1) else (yr, wk - 1)
    nextWeek = if wk >= 53 then (yr + 1, wk - 53 + 1) else (yr, wk + 1)
    startDay = fromMondayStartWeek yr wk 1
    endDay = addGregorianDurationClip calendarDay $ fromMondayStartWeek yr wk 7
    linked =
      [ ("prev week", DateByWeekR (fst prevWeek) (snd prevWeek)),
        ("last year", DateByWeekR (yr - 1) wk),
        ("first day", DateByDayR startDay),
        ("next year", DateByWeekR (yr + 1) wk),
        ("next week", DateByWeekR (fst nextWeek) (snd nextWeek))
      ]
