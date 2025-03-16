module Korrvigs.Web.Collections where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Metadata.Collections
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Note.Loc
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Note as Note
import Korrvigs.Web.PhotoSwipe (PhotoswipeEntry (..))
import qualified Korrvigs.Web.PhotoSwipe as PhotoSwipe
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Widget
import qualified Korrvigs.Web.Widgets as Widgets
import Opaleye hiding (not, null)
import Text.Blaze (textValue, toMarkup, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Yesod

getColR :: Handler Html
getColR = do
  render <- getUrlRender
  let hdFav = Html.a "Favourites" ! Attr.href (textValue $ render $ ColFavouriteR [])
  favs <- displayFavTree 1 0 hdFav [] =<< colCatTree Favourite []
  let hdMisc = Html.a "Miscellaneous" ! Attr.href (textValue $ render $ ColMiscR [])
  miscs <- displayMiscTree ColMiscR (const $ Widgets.headerSymbol "•") 1 0 hdMisc [] =<< colCatTree MiscCollection []
  let hdGal = Html.a "Gallery" ! Attr.href (textValue $ render $ ColGalR [])
  gals <- displayMiscTree ColGalR (const $ Widgets.headerSymbol "📷") 1 0 hdGal [] =<< colCatTree GalleryCollection []
  let hdTask = Html.a "Task sets" ! Attr.href (textValue $ render $ ColTaskR [])
  tsks <- displayMiscTree ColTaskR (const $ Widgets.headerSymbol "✔") 1 0 hdTask [] =<< colCatTree TaskSet []
  defaultLayout $ do
    setTitle "Collections"
    Rcs.entryStyle
    Widgets.sectionLogic
    [whamlet|
      <h1>Collections
      ^{favs}
      ^{miscs}
      ^{gals}
      ^{tsks}
    |]

displayFavTree :: Int -> Int -> Html -> [Text] -> ColTree -> Handler Widget
displayFavTree = displayMiscTree ColFavouriteR $ const $ Widget.headerSymbol "★"

displayMiscTree :: ([Text] -> Route WebData) -> (Int -> Widget) -> Int -> Int -> Html -> [Text] -> ColTree -> Handler Widget
displayMiscTree = displayTreeImpl mkLeafs
  where
    mkLeafs :: Int -> [(Id, Maybe Text)] -> Handler Widget
    mkLeafs _ ents =
      pure
        [whamlet|
        <ul>
          $forall (i,title) <- ents
            <li>
              <a href=@{EntryR (WId i)}>
                $maybe t <- title
                  #{t}
                $nothing
                  @#{unId i}
      |]

displayTreeImpl ::
  (Int -> [(Id, Maybe Text)] -> Handler Widget) ->
  ([Text] -> Route WebData) ->
  (Int -> Widget) ->
  Int ->
  Int ->
  Html ->
  [Text] ->
  ColTree ->
  Handler Widget
displayTreeImpl mkLeafs mkUrl symbols lvl threshold cat rprefix favs = do
  let entries = favs ^. colEntries
  subs <- forM (M.toList $ favs ^. colSubs) $ \(subHd, sb) -> do
    render <- getUrlRender
    let url = mkUrl $ reverse $ subHd : rprefix
    let subH = Html.a (toMarkup subHd) ! Attr.href (textValue $ render url)
    displayTreeImpl mkLeafs mkUrl symbols (lvl + 1) threshold subH (subHd : rprefix) sb
  leafs <- mkLeafs lvl entries
  let content =
        [whamlet|
    ^{leafs}
    $forall sb <- subs
      ^{sb}
  |]
  pure $ void $ Widgets.mkSection lvl [("class", "collapsed") | lvl > threshold] [] (header lvl) content
  where
    header :: Int -> Widget
    header 0 = [whamlet|<h1> ^{symbols 0} ^{cat}|]
    header 1 = [whamlet|<h2> ^{symbols 1} ^{cat}|]
    header 2 = [whamlet|<h3> ^{symbols 2} ^{cat}|]
    header 3 = [whamlet|<h4> ^{symbols 3} ^{cat}|]
    header 4 = [whamlet|<h5> ^{symbols 4} ^{cat}|]
    header n = [whamlet|<h6> ^{symbols n} ^{cat}|]

getColFavouriteR :: [Text] -> Handler Html
getColFavouriteR prefix = do
  render <- getUrlRender
  favs <- displayFavTree 0 0 (mkTitle (render . ColFavouriteR) "Favourites" rprefix) rprefix =<< colTree Favourite prefix True
  defaultLayout $ do
    setTitle $ toMarkup title
    Rcs.entryStyle
    Widgets.sectionLogic
    favs
  where
    rprefix = reverse prefix
    title = case rprefix of
      [] -> "Favourites"
      (hd : _) -> hd

mkTitle :: ([Text] -> Text) -> Text -> [Text] -> Html
mkTitle renderUrl base rprefix = case rprefix of
  [] -> toMarkup base
  (lp : prs) -> buildTitle prs <> toMarkup lp
  where
    buildTitle :: [Text] -> Html
    buildTitle [] = mempty
    buildTitle (hd : rpr) =
      let rec = buildTitle rpr
       in let lnk = Html.a (toMarkup hd) ! Attr.href (textValue $ renderUrl $ reverse $ hd : rpr)
           in rec <> lnk <> " > "

getColMiscR :: [Text] -> Handler Html
getColMiscR prefix = do
  render <- getUrlRender
  miscs <- displayMiscTree ColMiscR (const $ Widget.headerSymbol "•") 0 0 (mkTitle (render . ColMiscR) "Miscellaneous" rprefix) rprefix =<< colTree MiscCollection prefix True
  defaultLayout $ do
    setTitle $ toMarkup title
    Rcs.entryStyle
    Widgets.sectionLogic
    miscs
  where
    rprefix = reverse prefix
    title = case rprefix of
      [] -> "Miscellaneous"
      (lp : _) -> lp

getColGalR :: [Text] -> Handler Html
getColGalR prefix = do
  render <- getUrlRender
  let titleH = mkTitle (render . ColGalR) "Gallery" rprefix
  subTree <- colCatTree GalleryCollection prefix
  subs <- displayMiscTree ColGalR (const $ Widget.headerSymbol "📷") 1 0 "Sub galleries" rprefix subTree
  pictures <- rSelect $ orderBy (ascNullsFirst snd <> asc fst) $ do
    (i, _) <- selectCol GalleryCollection prefix False
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== i
    pure (i, entry ^. sqlEntryDate)
  entries <- mapM mkEntry pictures
  photoswipe <- PhotoSwipe.photoswipe $ catMaybes entries
  defaultLayout $ do
    setTitle $ toMarkup title
    Rcs.entryStyle
    Widgets.sectionLogic
    PhotoSwipe.photoswipeHeader
    [whamlet|
      <h1>📷 ^{titleH}
      $if not (nullTree subTree)
        ^{subs}
      <div #common-gallery>
        ^{photoswipe}
    |]
  where
    rprefix = reverse prefix
    title = case rprefix of
      [] -> "Gallery"
      (lp : _) -> lp
    mkEntry :: (Id, Maybe ZonedTime) -> Handler (Maybe PhotoswipeEntry)
    mkEntry (i, dt) =
      PhotoSwipe.miniatureEntry (localDay . zonedTimeToLocalTime <$> dt) i

getColTaskR :: [Text] -> Handler Html
getColTaskR prefix = do
  tree <- colTree TaskSet prefix True
  render <- getUrlRender
  let titleH = mkTitle (render . ColTaskR) "Task sets" rprefix
  widget <- displayTreeImpl mkLeafs ColTaskR (const $ Widgets.headerSymbol "✔") 0 0 titleH prefix tree
  defaultLayout $ do
    setTitle $ toMarkup title
    Rcs.entryStyle
    Widgets.sectionLogic
    Rcs.checkboxCode
    widget
  where
    rprefix = reverse prefix
    title = case rprefix of
      [] -> "Task sets"
      (lp : _) -> lp
    mkLeafs :: Int -> [(Id, Maybe Text)] -> Handler Widget
    mkLeafs tlvl entries = do
      let lvl = tlvl + 1
      widgets <- forM entries $ \(i, titleText) -> do
        (widget, _) <- Note.embedBody i lvl
        tsk <- loadTask i
        taskW <- Widgets.taskWidget i (SubLoc []) tsk
        let titleT = fromMaybe (unId i) titleText
        let titleW = [whamlet|<a href=@{EntryR $ WId i}>#{titleT}|]
        let t = header lvl [whamlet|^{Widgets.headerSymbol "📖"} ^{taskW} ^{titleW}|]
        pure . void $ Widgets.mkSection lvl [("class", "collapsed")] [] t widget
      pure $ mconcat widgets
    header :: Int -> Widget -> Widget
    header 0 content = [whamlet|<h1> ^{content}|]
    header 1 content = [whamlet|<h2> ^{content}|]
    header 2 content = [whamlet|<h3> ^{content}|]
    header 3 content = [whamlet|<h4> ^{content}|]
    header 4 content = [whamlet|<h5> ^{content}|]
    header _ content = [whamlet|<h6> ^{content}|]
