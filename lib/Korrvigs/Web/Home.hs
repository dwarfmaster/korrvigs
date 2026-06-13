module Korrvigs.Web.Home (getHomeR, getEvents) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Note.Loc
import Korrvigs.Query
import Korrvigs.Utils
import Korrvigs.Utils.Time
import Korrvigs.Web.Actions
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Note as Note
import qualified Korrvigs.Web.JS.FullCalendar as FC
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Widgets
import Yesod hiding (joinPath)

getEvents :: ZonedTime -> Handler [EntryRowR]
getEvents today = do
  let month = CalendarDiffTime 1 $ secondsToNominalDiffTime 0
  let start = addCalendar (scaleCalendarDiffTime (-1) month) today
  let end = addCalendar month today
  let query =
        def
          & queryAfter ?~ start
          & queryBefore ?~ end
          & queryKind ?~ queryFromKind Event
          & querySort .~ (ByDate, SortAsc)
  rSelect $ fst <$> compile query (const $ pure ())

eventsWidget :: Handler Widget
eventsWidget = do
  time <- liftIO getCurrentZonedTime
  evs <- getEvents time
  entries <- mapM FC.entryToEvent evs
  dt <- liftIO getCurrentZonedTime
  let day = localDay $ zonedTimeToLocalTime dt
  render <- getUrlRender
  let url = render $ DateByDayR day
  let today =
        FC.CalendarEvent
          { FC._evTitle = "Today",
            FC._evStart = dt,
            FC._evEnd = Nothing,
            FC._evAllDay = Just True,
            FC._evUrl = Just url,
            FC._evColor = Just "var(--base0E)"
          }
  widget <- FC.widget $ today : catMaybes entries
  pure $ do
    FC.header
    widget

favouritesWidget :: Handler Widget
favouritesWidget = fromMaybeT notFoundWidget $ do
  let i = MkId "Favourites"
  entry <- hoistLift $ load i
  note <- hoistMaybe $ entry ^? entryKindData . _NoteD
  msubL <- lift Note.getOpenParam
  (widget, _) <- lift $ Note.embedOpen 1 note (MkId "", DeepEmbedLoc []) msubL
  pure widget
  where
    notFoundWidget :: Widget
    notFoundWidget = [whamlet|<p>No entry with id Favourites|]

displayHome :: Handler Html
displayHome = do
  let actionsHd = [whamlet|<h2> ^{Widgets.headerSymbol "⊕"} Actions|]
  actions <- actionsWidget TargetHome
  let favsHd =
        [whamlet|
    <h2> ^{Widgets.headerSymbol "★"}
      <a href=@{EntryR $ WId $ MkId "Favourites"}>
        Favourites
  |]
  favs <- favouritesWidget
  let eventsHd = [whamlet|<h2> ^{Widgets.headerSymbol "🕑"} Calendar|]
  evs <- eventsWidget
  defaultLayout $ do
    setTitle "Korrvigs's Home"
    setDescriptionIdemp "Korrvigs home page"
    Rcs.entryStyle CssR
    Rcs.formsStyle CssR
    Widgets.sectionLogic
    Rcs.checkboxCode StaticR
    toWidget [julius|checkboxCleanSpans();|]
    [whamlet|
    <h1>Welcome to Korrvigs
    ^{void $ Widgets.mkSection 1 [] [] actionsHd actions}
    ^{void $ Widgets.mkSection 1 [] [] favsHd favs}
    ^{void $ Widgets.mkSection 1 [] [] eventsHd evs}
  |]

getHomeR :: Handler Html
getHomeR = displayHome
