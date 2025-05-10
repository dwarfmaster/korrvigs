module Korrvigs.Web.Widgets where

import Control.Lens
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Metadata.Task
import Korrvigs.Note.Loc
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

headerSymbol :: Text -> Widget
headerSymbol s = [whamlet|<span .section-symbol>#{s}|]

mkSection :: Int -> [(Text, Text)] -> [(Text, Text)] -> Widget -> Widget -> WidgetFor WebData Text
mkSection lvl secAttrs divAttrs header content = do
  let htmlLvl = if lvl > 5 then 6 else lvl + 1
  let lvlClass = T.pack $ "level" <> show htmlLvl
  let secAttrsFinal = updClasses [lvlClass] secAttrs
  let divAttrsFinal = updClasses ["section-content"] divAttrs
  i <- newIdent
  [whamlet|
    <section ##{i} *{secAttrsFinal}>
      ^{header}
      <div *{divAttrsFinal}>
        ^{content}
  |]
  pure i
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

taskWidget :: Id -> SubLoc -> Maybe Task -> Handler Widget
taskWidget _ _ Nothing = pure mempty
taskWidget i subL (Just tsk) = do
  public <- isPublic
  pure $ do
    spanId <- newIdent
    let loc = LocTask $ TaskLoc subL
    unless public $
      if null (subL ^. subOffsets)
        then
          toWidget
            [julius|
          setupTopTask("@{EntryMtdtR (WId i)}", #{spanId}, #{unId i})
        |]
        else
          toWidget
            [julius|
          setupTask("@{NoteSubR (WId i) (WLoc loc)}", #{spanId})
        |]
    [whamlet|
      <span ##{spanId} .task-span .#{status}>
        ^{lbl}
    |]
  where
    lbl = tsk ^. tskStatusName
    status :: Text
    status = case tsk ^. tskStatus of
      TaskTodo -> "task-todo"
      TaskImportant -> "task-important"
      TaskOngoing -> "task-ongoing"
      TaskBlocked -> "task-blocked"
      TaskDone -> "task-done"
      TaskDont -> "task-dont"
