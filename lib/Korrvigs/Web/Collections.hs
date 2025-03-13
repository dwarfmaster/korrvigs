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
import Yesod

displayFavTree :: Int -> Text -> [Text] -> ColTree -> Handler Widget
displayFavTree lvl cat rprefix favs = do
  let entries = favs ^. colEntries
  subs <- forM (M.toList $ favs ^. colSubs) $ \(subHd, sub) ->
    displayFavTree (lvl + 1) subHd (subHd : rprefix) sub
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
  pure $ void $ Widgets.mkSection lvl [("class", "collapsed") | lvl > 1] [] (header lvl) content
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
  favs <- displayFavTree 0 hdTree (reverse prefix) =<< colTree Favourite prefix True
  defaultLayout $ do
    setTitle "Favourite page"
    Rcs.entryStyle
    Widgets.sectionLogic
    favs
