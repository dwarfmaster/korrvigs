module Korrvigs.Web.Home (getHomeR) where

import Control.Monad
import Korrvigs.Metadata.Collections
import Korrvigs.Web.Actions
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Collections as Cols
import qualified Korrvigs.Web.Ressources as Rcs
import qualified Korrvigs.Web.Widgets as Widgets
import Text.Blaze (textValue, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Yesod hiding (joinPath)

displayHome :: Handler Html
displayHome = do
  let actionsHd = [whamlet|<h2> ^{Widgets.headerSymbol "⊕"} Actions|]
  actions <- actionsWidget TargetHome
  render <- getUrlRender
  let hd = Html.a "Favourites" ! Attr.href (textValue $ render $ ColR ["Favourite"])
  favs <- Cols.displayTree Nothing 1 1 hd ["Favourite"] =<< colTree MiscCollection ["Favourite"] True
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
  |]

getHomeR :: Handler Html
getHomeR = displayHome
