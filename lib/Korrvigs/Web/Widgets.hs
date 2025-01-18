module Korrvigs.Web.Widgets where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Web.Backend
import Yesod

headerSymbol :: Text -> Widget
headerSymbol s = [whamlet|<span .section-symbol>#{s}|]

mkSection :: Int -> [(Text, Text)] -> [(Text, Text)] -> Widget -> Widget -> Widget
mkSection lvl secAttrs divAttrs header content =
  let lvlClass = T.pack $ "level" <> show (lvl + 1)
   in let secAttrsFinal = updClasses [lvlClass] secAttrs
       in let divAttrsFinal = updClasses ["section-content"] divAttrs
           in [whamlet|
    <section *{secAttrsFinal}>
      ^{header}
      <div *{divAttrsFinal}>
        ^{content}
  |]
  where
    updClasses' :: [Text] -> Map Text Text -> Map Text Text
    updClasses' clss attrs = case M.lookup "class" attrs of
      Nothing -> M.insert "class" (T.intercalate " " clss) attrs
      Just nclss -> M.insert "class" (T.intercalate " " clss <> " " <> nclss) attrs
    updClasses clss = M.toList . updClasses' clss . M.fromList

sectionLogic :: Widget
sectionLogic =
  toWidget
    [julius|
    var syms = document.querySelectorAll('.section-symbol')
    for(let sym = 0; sym < syms.length; sym++) {
      syms[sym].addEventListener("click", function () {
        syms[sym].parentElement.parentElement.classList.toggle("collapsed")
      })
    }
  |]
