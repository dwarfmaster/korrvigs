module Korrvigs.Web.Home (getHomeR, getEvents) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Contact
import Korrvigs.Monad
import Korrvigs.Note.Loc
import Korrvigs.Query
import Korrvigs.Utils
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Time
import Korrvigs.Web.Actions
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Note as Note
import qualified Korrvigs.Web.JS.FullCalendar as FC
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Widgets
import Opaleye
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

birthdaysWidget :: Handler (Maybe Widget)
birthdaysWidget = do
  time <- liftIO getCurrentTime
  let (_, month, day) = toGregorian $ utctDay time
  let bday = BirthDay month day
  birthers <- rSelect $ do
    bdaymtdt <- selectTable entriesMetadataTable
    where_ $ bdaymtdt ^. sqlKey .== sqlStrictText (mtdtSqlName BirthDayMtdt)
    where_ $ bdaymtdt ^. sqlValue .== sqlValueJSONB bday
    bdayYear <- sqlJsonToNum <$> selectMtdt BirthYear (bdaymtdt ^. sqlEntry)
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryId .== bdaymtdt ^. sqlEntry
    pure (entry ^. sqlEntryName, entry ^. sqlEntryTitle, bdayYear)
  birthersWithAge <- forM birthers $ \(nm, title, yr) -> case yr of
    Nothing -> pure (nm, title, Nothing)
    Just y -> (nm,title,) . Just <$> computeAge (floor (y :: Double)) (Just bday)
  case birthers of
    [] -> pure Nothing
    _ -> do
      pure $
        Just $
          [whamlet|
          <ul>
            $forall (nm, title, mage) <- birthersWithAge
              <li>
                <a href=@{EntryR (WId nm)}>
                  #{fromMaybe ("@" <> unId nm) title}
                $maybe age <- mage
                  #{mconcat [" (", T.pack (show age), " years old)"]}
        |]

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
  births <- birthdaysWidget
  let birthsHd = [whamlet|<h2> ^{Widgets.headerSymbol "🎉"} Birthdays|]
  cssR <- mkCss
  defaultLayout $ do
    setTitle "Korrvigs's Home"
    setDescriptionIdemp "Korrvigs home page"
    Rcs.entryStyle cssR
    Rcs.formsStyle cssR
    Widgets.sectionLogic
    Rcs.checkboxCode StaticR
    toWidget [julius|checkboxCleanSpans();|]
    [whamlet|
    <h1>Welcome to Korrvigs
    ^{void $ Widgets.mkSection 1 [] [] actionsHd actions}
    $maybe bwidget <- births
      ^{void $ Widgets.mkSection 1 [] [] birthsHd bwidget}
    ^{void $ Widgets.mkSection 1 [] [] favsHd favs}
    ^{void $ Widgets.mkSection 1 [] [] eventsHd evs}
  |]

getHomeR :: Handler Html
getHomeR = displayHome
