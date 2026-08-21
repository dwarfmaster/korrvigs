module Korrvigs.Web.Widgets where

import Control.Lens
import Control.Monad
import Data.Either.Extra (eitherToMaybe)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Contact
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Note
import Korrvigs.Note.Languages
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Opaleye hiding (not, null)
import Skylighting hiding (lookupSyntax)
import Text.Blaze hiding ((!))
import qualified Text.Blaze as Blz
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Yesod hiding (Attr, Field)
import Yesod.Static

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
        if(window.hasOwnProperty("updateFolded")) {
          updateFolded = updateFolded.filter((mpUpd) => !mpUpd());
        }
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

checkBox :: TaskStatus -> Route WebData -> Handler (Html, Widget, Text)
checkBox ck postRoute = do
  render <- getUrlRender
  public <- isPublic
  cid <- newIdent
  let todoUrl = render $ checkImg TaskTodo
  let importantUrl = render $ checkImg TaskImportant
  let ongoingUrl = render $ checkImg TaskOngoing
  let blockedUrl = render $ checkImg TaskBlocked
  let doneUrl = render $ checkImg TaskDone
  let dontUrl = render $ checkImg TaskDont
  let postUrl = render postRoute
  let h =
        applyAttr (Attr.id $ textValue cid) $
          applyAttr (Attr.src $ textValue $ render $ checkImg ck) $
            applyAttr (Attr.class_ "checkBox") Html.img
  let w = toWidget [julius|setupCheckbox(#{postUrl}, #{todoUrl}, #{importantUrl}, #{ongoingUrl}, #{blockedUrl}, #{doneUrl}, #{dontUrl}, #{cid});|]
  pure (h, if public then mempty else w, cid)
  where
    checkImg :: TaskStatus -> Route WebData
    checkImg TaskTodo = StaticR $ StaticRoute ["icons", "checkbox-todo.svg"] []
    checkImg TaskImportant = StaticR $ StaticRoute ["icons", "checkbox-important.svg"] []
    checkImg TaskOngoing = StaticR $ StaticRoute ["icons", "checkbox-ongoing.svg"] []
    checkImg TaskBlocked = StaticR $ StaticRoute ["icons", "checkbox-blocked.svg"] []
    checkImg TaskDone = StaticR $ StaticRoute ["icons", "checkbox-done.svg"] []
    checkImg TaskDont = StaticR $ StaticRoute ["icons", "checkbox-dont.svg"] []

checkBoxDWIM :: Id -> Maybe Text -> Handler Widget
checkBoxDWIM _ Nothing = pure mempty
checkBoxDWIM i (Just tsName) = case parseStatusName tsName of
  Just ts -> do
    (h, w, _) <- checkBox ts (EntryMtdtR $ WId i)
    pure $ w >> toWidget h
  Nothing -> pure mempty

applyAttr :: Attribute -> Html -> Html
applyAttr attr html = html Blz.! attr

openIcon :: Widget
openIcon =
  [whamlet|
  <img width=16 height=16 style="display: inline; vertical-align: -10%;" src=@{StaticR $ StaticRoute ["icons", "open-white.png"] []}>
|]

embedPdf :: Route WebData -> Widget
embedPdf url =
  [whamlet|<embed src=@{url} width=100% height=700 type="application/pdf">|]

skyStyle :: Html
skyStyle = Html.style (toMarkup $ styleToCss zenburn) ! Attr.type_ "text/css"

skyContent :: Attr -> Text -> Html
skyContent attr codeSource = case parseResult of
  Nothing -> Html.pre $ toMarkup codeSource
  Just tokens -> formatHtmlBlock cfg tokens
  where
    cfg =
      FormatOptions
        { numberLines = not $ T.null $ attr ^. attrId,
          startNumber = 1,
          lineAnchors = False,
          titleAttributes = False,
          codeClasses = [],
          containerClasses = [],
          lineIdPrefix = (attr ^. attrId) <> "--",
          ansiColorLevel = ANSITrueColor
        }
    tokenizerCfg =
      TokenizerConfig
        { syntaxMap = defaultSyntaxMap,
          traceOutput = False
        }
    parseResult :: Maybe [SourceLine]
    parseResult = do
      syntax <- getAlt $ mconcat $ Alt . lookupSyntax <$> attr ^. attrClasses
      eitherToMaybe $ tokenize tokenizerCfg syntax codeSource
    lookupSyntax :: Text -> Maybe Syntax
    lookupSyntax = lookupSky >=> flip M.lookup defaultSyntaxMap
    lookupSky :: Text -> Maybe Text
    lookupSky l = languagesMap ^? at l . _Just . langSkylight . _Just

-- Assumes startDay and endDay are less than one year appart, otherwise it is not
-- possible to compute someone's age in the range.
birthdaysWidget :: Day -> Day -> Handler (Maybe Widget)
birthdaysWidget startDay endDay = do
  birthers <- rSelect $ do
    bdaymtdt <- selectTable entriesMetadataTable
    where_ $ bdaymtdt ^. sqlKey .== sqlStrictText (mtdtSqlName BirthDayMtdt)
    where_ $
      matchNullable (sqlBool False) cond $
        sqlJsonToText $
          toNullable $
            bdaymtdt ^. sqlValue
    bdayYear <- sqlJsonToNum <$> selectMtdt BirthYear (bdaymtdt ^. sqlEntry)
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryId .== bdaymtdt ^. sqlEntry
    pure (entry ^. sqlEntryName, entry ^. sqlEntryTitle, (bdayYear, bdaymtdt ^. sqlValue))
  let birthersPrepared =
        sortBy (\bd1 bd2 -> compare (bd1 ^? _3 . _Just . _2) (bd2 ^? _3 . _Just . _2)) $
          map (_3 %~ \(yr, bdayJS) -> (,) <$> yr <*> fromJSONM bdayJS) birthers
  case birthers of
    [] -> pure Nothing
    _ -> do
      pure $
        Just $
          [whamlet|
          <ul>
            $forall (nm, title, mage) <- birthersPrepared
              <li>
                <a href=@{EntryR (WId nm)}>
                  #{fromMaybe ("@" <> unId nm) title}
                $maybe (byear,bday) <- mage
                  #{mconcat [" (", T.pack (show $ ageAt byear bday), " years old on ", rdrBday bday, ")"]}
        |]
  where
    (year, month, dnum) = toGregorian startDay
    (endYear, endMonth, endDnum) = toGregorian endDay
    startBirth = renderBirthday $ BirthDay month dnum
    endBirth = renderBirthday $ BirthDay endMonth endDnum
    birthOp = if month == December then (.||) else (.&&)
    cond bday = (bday .>= sqlStrictText startBirth) `birthOp` (bday .<= sqlStrictText endBirth)
    birthYear :: BirthDay -> Year
    birthYear (BirthDay mth day) =
      if mth < month || (mth == month && day < dnum) then endYear else year
    rdrBday :: BirthDay -> Text
    rdrBday bday@(BirthDay mth day) =
      T.pack $ formatTime defaultTimeLocale "%A %d, %B" $ fromGregorian (birthYear bday) mth day
    ageAt :: Double -> BirthDay -> Integer
    ageAt byear bday@(BirthDay mth day) =
      snd $ computeAgeAt (floor byear) (Just bday) $ fromGregorian (birthYear bday) mth day
