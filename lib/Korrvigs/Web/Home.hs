module Korrvigs.Web.Home (getHomeR) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Collections
import Korrvigs.Monad
import Korrvigs.Query
import Korrvigs.Utils.Time
import Korrvigs.Web.Actions
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Collections as Cols
import qualified Korrvigs.Web.FullCalendar as FC
import qualified Korrvigs.Web.Ressources as Rcs
import qualified Korrvigs.Web.Widgets as Widgets
import Text.Blaze (textValue, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Yesod hiding (joinPath)

getEvents :: Handler [(EntryRow, Maybe Text)]
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
  rSelect $ do
    entry <- compile query
    title <- selectTextMtdt Title $ entry ^. sqlEntryName
    pure (entry, title)

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

displayHome :: Handler Html
displayHome = do
  let actionsHd = [whamlet|<h2> ^{Widgets.headerSymbol "⊕"} Actions|]
  actions <- actionsWidget TargetHome
  render <- getUrlRender
  let hd = Html.a "Favourites" ! Attr.href (textValue $ render $ ColR ["Favourite"])
  favs <- Cols.displayTree Nothing 1 1 hd ["Favourite"] =<< colTree MiscCollection ["Favourite"] True
  let eventsHd = [whamlet|<h2> ^{Widgets.headerSymbol "🕑"} Calendar|]
  evs <- eventsWidget
  defaultLayout $ do
    setTitle "Korrvigs's Home"
    setDescriptionIdemp "Korrvigs home page"
    Rcs.entryStyle
    Rcs.formsStyle
    Widgets.sectionLogic
    [whamlet|
    <h1>Welcome to Korrvigs
    ^{void $ Widgets.mkSection 1 [] [] actionsHd actions}
    ^{favs}
    ^{void $ Widgets.mkSection 1 [] [] eventsHd evs}
  |]

getHomeR :: Handler Html
getHomeR = displayHome
