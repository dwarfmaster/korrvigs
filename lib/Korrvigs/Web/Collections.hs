module Korrvigs.Web.Collections where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Metadata.Collections
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Widgets
import Text.Blaze (textValue, toMarkup, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Yesod

getColR :: Handler Html
getColR = do
  render <- getUrlRender
  let hd = Html.a "Favourites" ! Attr.href (textValue $ render $ ColFavouriteR [])
  favs <- displayFavTree 1 0 hd [] =<< colCatTree Favourite []
  defaultLayout $ do
    setTitle "Collections"
    Rcs.entryStyle
    Widgets.sectionLogic
    [whamlet|
      <h1>Collections
      ^{favs}
    |]

displayFavTree :: Int -> Int -> Html -> [Text] -> ColTree -> Handler Widget
displayFavTree lvl threshold cat rprefix favs = do
  let entries = favs ^. colEntries
  subs <- forM (M.toList $ favs ^. colSubs) $ \(subHd, sub) -> do
    render <- getUrlRender
    let url = ColFavouriteR $ reverse $ subHd : rprefix
    let subH = Html.a (toMarkup subHd) ! Attr.href (textValue $ render url)
    displayFavTree (lvl + 1) threshold subH (subHd : rprefix) sub
  let content =
        [whamlet|
    <ul>
      $forall (i,title) <- entries
        <li>
          <a href=@{EntryR (WId i)}>
            $maybe t <- title
              #{t}
            $nothing
              @#{unId i}
    $forall sub <- subs
      ^{sub}
  |]
  pure $ void $ Widgets.mkSection lvl [("class", "collapsed") | lvl > threshold] [] (header lvl) content
  where
    header :: Int -> Widget
    header 0 = [whamlet|<h1> ^{Widgets.headerSymbol "★"} ^{cat}|]
    header 1 = [whamlet|<h2> ^{Widgets.headerSymbol "★"} ^{cat}|]
    header 2 = [whamlet|<h3> ^{Widgets.headerSymbol "★"} ^{cat}|]
    header 3 = [whamlet|<h4> ^{Widgets.headerSymbol "★"} ^{cat}|]
    header 4 = [whamlet|<h5> ^{Widgets.headerSymbol "★"} ^{cat}|]
    header _ = [whamlet|<h6> ^{Widgets.headerSymbol "★"} ^{cat}|]

getColFavouriteR :: [Text] -> Handler Html
getColFavouriteR prefix = do
  render <- getUrlRender
  favs <- displayFavTree 0 0 (mkTitle render) rprefix =<< colTree Favourite prefix True
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
    mkTitle :: (Route WebData -> Text) -> Html
    mkTitle render = case rprefix of
      [] -> "Favourites"
      (lp : prs) -> buildTitle render prs <> toMarkup lp
    buildTitle :: (Route WebData -> Text) -> [Text] -> Html
    buildTitle _ [] = mempty
    buildTitle render (hd : rpr) =
      let rec = buildTitle render rpr
       in let lnk = Html.a (toMarkup hd) ! Attr.href (textValue $ render $ ColFavouriteR $ reverse $ hd : rpr)
           in rec <> lnk <> " > "
