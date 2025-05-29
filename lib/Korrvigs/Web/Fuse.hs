module Korrvigs.Web.Fuse where

import Control.Lens hiding ((.=))
import Data.Maybe
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Text.Blaze.Renderer.Text (renderMarkup)
import Yesod

data FuseItem = FuseItem
  { _itDisplay :: Html,
    _itText :: Text,
    _itSubText :: Text
  }

makeLenses ''FuseItem

instance ToJSON FuseItem where
  toJSON it =
    object
      [ "display" .= renderMarkup (it ^. itDisplay),
        "text" .= (it ^. itText),
        "subtext" .= (it ^. itSubText)
      ]

itemFromEntry :: (Id, Maybe Text) -> Handler FuseItem
itemFromEntry (i, title) = do
  render <- getUrlRenderParams
  public <- isPublic
  pure $
    FuseItem
      { _itDisplay =
          [hamlet|
      $if public
        ^{plain}
      $else
        <a href=@{EntryR $ WId i}>
          ^{plain}
    |]
            render,
        _itText = fromMaybe "" title,
        _itSubText = unId i
      }
  where
    plain = case title of
      Nothing -> [hamlet|@#{unId i}|]
      Just t -> [hamlet|#{t}|]

logic :: JavascriptUrl url
logic =
  [julius|
  function setupFuse(inputId, listId, items) {
    var lst = document.getElementById(listId)
    const resetList = function() {
      lst.innerHTML = ''
      items.forEach(item => {
        var li = document.createElement('li')
        li.innerHTML = item.display
        lst.appendChild(li)
      })
    }
    const options = {
      ignoreDiacritics: true,
      shouldSort: true,
      keys: [{ name: 'text', weight: 2 }, 'subtext']
    }
    const fuse = new Fuse(items, options)
    document.getElementById(inputId).addEventListener("input", (e) => {
      if(e.target.value) {
        const result = fuse.search(e.target.value)
        lst.innerHTML = ''
        result.forEach(item => {
          var li = document.createElement('li')
          li.innerHTML = item.item.display
          lst.appendChild(li)
        })
      } else {
        resetList()
      }
    })
    resetList()
  }
|]

style :: CssUrl url
style =
  [cassius|
  .fuse
    input
      width: 100%
      box-sizing: border-box
      background-color: var(--base07)
      color: var(--base00)
      outline-style: none
      outline-width: 2px
      outline-color: var(--base0F)
      border-style: none
    ul
      list-style: none
|]

header :: Widget
header = do
  Rcs.fuse StaticR
  toWidget logic
  toWidget style

widget :: [FuseItem] -> Handler Widget
widget items = do
  inputId <- newIdent
  listId <- newIdent
  pure $ do
    toWidget [julius|setupFuse(#{inputId}, #{listId}, #{rawJSON items})|]
    [whamlet|
      <div .fuse>
        <input type=text ##{inputId}>
        <ul ##{listId}>
    |]
