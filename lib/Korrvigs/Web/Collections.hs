module Korrvigs.Web.Collections where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Metadata.Collections
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Widgets
import Text.Blaze (toMarkup)
import Yesod

getColR :: Handler Html
getColR = do
  favs <- displayFavTree 1 0 "Favourites" [] =<< colCatTree Favourite []
  defaultLayout $ do
    setTitle "Collections"
    Rcs.entryStyle
    Widgets.sectionLogic
    [whamlet|
      <h1>Collections
      ^{favs}
    |]

displayFavTree :: Int -> Int -> Text -> [Text] -> ColTree -> Handler Widget
displayFavTree lvl threshold cat rprefix favs = do
  let entries = favs ^. colEntries
  subs <- forM (M.toList $ favs ^. colSubs) $ \(subHd, sub) ->
    displayFavTree (lvl + 1) threshold subHd (subHd : rprefix) sub
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
    header 0 = [whamlet|<h1> ^{Widgets.headerSymbol "★"} ^{ref}|]
    header 1 = [whamlet|<h2> ^{Widgets.headerSymbol "★"} ^{ref}|]
    header 2 = [whamlet|<h3> ^{Widgets.headerSymbol "★"} ^{ref}|]
    header 3 = [whamlet|<h4> ^{Widgets.headerSymbol "★"} ^{ref}|]
    header 4 = [whamlet|<h5> ^{Widgets.headerSymbol "★"} ^{ref}|]
    header _ = [whamlet|<h6> ^{Widgets.headerSymbol "★"} ^{ref}|]
    ref :: Widget
    ref = [whamlet|<a href=@{ColFavouriteR (reverse rprefix)}>#{cat}|]

getColFavouriteR :: [Text] -> Handler Html
getColFavouriteR prefix = do
  let hdTree = if null prefix then "Favourites" else T.intercalate ">" prefix
  favs <- displayFavTree 0 0 hdTree (reverse prefix) =<< colTree Favourite prefix True
  defaultLayout $ do
    setTitle $ toMarkup hdTree
    Rcs.entryStyle
    Widgets.sectionLogic
    favs
