module Korrvigs.Web.Home (getHomeR) where

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
import Korrvigs.Query
import Korrvigs.Utils
import Korrvigs.Utils.Time
import Korrvigs.Web.Actions
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Note as Note
import qualified Korrvigs.Web.FullCalendar as FC
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Widgets
import Yesod hiding (joinPath)

getEvents :: Handler [EntryRowR]
getEvents = do
  today <- liftIO getCurrentZonedTime
  let month = CalendarDiffTime 1 $ secondsToNominalDiffTime 0
  let start = addCalendar (scaleCalendarDiffTime (-1) month) today
  let end = addCalendar month today
  let query =
        def
          & queryAfter ?~ start
          & queryBefore ?~ end
          & queryKind ?~ Event
          & querySort .~ (ByDate, SortAsc)
  rSelect $ fst <$> compile query (const $ pure ())

eventsWidget :: Handler Widget
eventsWidget = do
  evs <- getEvents
  entries <- mapM FC.entryToEvent evs
  dt <- liftIO getCurrentZonedTime
  let today = FC.CalendarEvent "Today" dt Nothing Nothing (Just True) (Just "var(--base0E)")
  widget <- FC.widget $ today : catMaybes entries
  pure $ do
    FC.header
    widget

favouritesWidget :: Handler Widget
favouritesWidget = fromMaybeT notFoundWidget $ do
  let i = MkId "Favourites"
  entry <- hoistLift $ load i
  note <- hoistMaybe $ entry ^? entryKindData . _NoteD
  (widget, _) <- lift $ Note.embed 1 note
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
    Rcs.entryStyle
    Rcs.formsStyle
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
