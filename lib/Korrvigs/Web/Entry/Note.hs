module Korrvigs.Web.Entry.Note (content, embed, editButton) where

import Control.Lens
import Control.Monad
import Control.Monad.Loops (whileJust)
import Control.Monad.State
import Data.Array
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Note hiding (code, task)
import qualified Korrvigs.Web.Ace as Ace
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Calendar as Cal
import qualified Korrvigs.Web.Entry.Event as Event
import qualified Korrvigs.Web.Entry.File as File
import qualified Korrvigs.Web.Entry.Link as Link
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Wdgs
import Yesod hiding (Attr, get, (.=))
import Yesod.Static hiding (embed)

data CompileState = CState
  { _noteCounter :: Int,
    _notes :: [(Int, [Block])],
    _topLevel :: Bool,
    _hdRootLevel :: Int,
    _currentLevel :: Int,
    _embedded :: Bool,
    _currentEntry :: Id,
    _subLoc :: SubLoc,
    _subCount :: Int,
    _codeCount :: Int,
    _checkboxCount :: Int,
    _openedSub :: SubLoc,
    _currentHeaderId :: Maybe Text
  }

makeLenses ''CompileState

initCState :: Id -> CompileState
initCState i = CState 1 [] True 0 0 False i (SubLoc []) 0 0 0 (SubLoc []) Nothing

type CompileM = StateT CompileState Handler

runCompile :: CompileState -> CompileM Widget -> Handler Widget
runCompile = flip evalStateT

pushSide :: [Block] -> CompileM Int
pushSide side = do
  counter <- use noteCounter
  notes %= (:) (counter, side)
  noteCounter %= (+ 1)
  pure counter

asNested :: CompileM a -> CompileM a
asNested act = do
  tl <- use topLevel
  topLevel .= False
  r <- act
  topLevel .= tl
  pure r

popNote :: CompileM (Maybe (Int, [Block]))
popNote =
  use notes >>= \case
    [] -> pure Nothing
    (n : nts) -> notes .= nts >> pure (Just n)

withLevel :: Int -> CompileM a -> CompileM a
withLevel lvl act = do
  prev <- use currentLevel
  currentLevel .= lvl
  r <- act
  currentLevel .= prev
  pure r

embed :: Int -> Note -> Handler Widget
embed lvl note = do
  let path = note ^. notePath
  mmd <- readNote path
  case mmd of
    Left err ->
      pure
        [whamlet|
        <p>
          Failed to load
          <em>#{path}
          :
        <code>
          #{err}
      |]
    Right md -> do
      let isEmbedded = lvl > 0
      subL <- fmap parseLoc <$> lookupGetParam "open"
      let st' =
            initCState (note ^. noteEntry . name)
              & hdRootLevel .~ lvl
              & currentLevel .~ lvl
              & embedded .~ isEmbedded
      let st = case subL of
            Just (Right (LocSub loc)) -> st' & openedSub .~ loc
            Just (Right (LocCode loc)) -> st' & openedSub .~ (loc ^. codeSub)
            _ -> st'
      markdown <- runCompile st $ compileBlocks $ md ^. docContent
      pure $ do
        Ace.setup
        Rcs.mathjax StaticR
        Rcs.checkboxCode
        [whamlet|
          <p .checks-top>
            ^{checksDisplay $ md ^. docChecks}
        |]
        markdown
        unless isEmbedded Wdgs.sectionLogic

content :: Note -> Handler Widget
content = embed 0

compileBlocks :: [Block] -> CompileM Widget
compileBlocks = fmap mconcat . mapM compileBlock

compileBlock :: Block -> CompileM Widget
compileBlock bk = do
  bkW <- compileBlock' bk
  tl <- use topLevel
  sideW <-
    if tl
      then do
        notes %= reverse
        whileJust popNote $ \(cnt, note) -> do
          noteW <- asNested $ compileBlocks note
          pure
            [whamlet|
          <div .sidenote>
            <span .sidenote-number>
              #{cnt}
            ^{noteW}
        |]
      else pure []
  pure $ bkW <> mconcat sideW

compileBlock' :: Block -> CompileM Widget
compileBlock' (Para inls) = do
  inlsW <- compileInlines inls
  pure [whamlet|<p>^{inlsW}|]
compileBlock' (LineBlock lns) = do
  linesW <- mapM compileInlines lns
  pure
    [whamlet|
  <p>
    $forall line <- linesW
      ^{line}
      <hr>
  |]
compileBlock' (CodeBlock attr code) = do
  let language = fromMaybe "text" $ find Ace.isLanguage $ attr ^. attrClasses
  ace <- lift $ Ace.preview code language
  divId <- newIdent
  buttonId <- newIdent
  entry <- use currentEntry
  subL <- use subLoc
  codeO <- use codeCount
  let loc = LocCode $ CodeLoc subL codeO
  hdId <- use currentHeaderId
  redirUrl <- lift $ aceRedirect entry hdId subL
  js <-
    lift $
      Ace.editOnClick
        buttonId
        divId
        language
        (NoteSubR (WId entry) $ WLoc loc)
        redirUrl
  codeCount %= (+ 1)
  pure $
    js
      >> [whamlet|
  <div ##{divId} .sourceCode *{compileAttr attr}>
    <div ##{buttonId} .edit-code>
      ✎
    ^{ace}
  |]
compileBlock' (BlockQuote bks) = do
  bksW <- compileBlocks bks
  pure [whamlet|<blockquote>^{bksW}|]
compileBlock' (OrderedList items) = do
  itemsW <- mapM compileBlocks items
  pure
    [whamlet|
  <ol>
    $forall item <- itemsW
      <li>^{item}
  |]
compileBlock' (BulletList items) = do
  itemsW <- mapM compileBlocks items
  pure
    [whamlet|
  <ul>
    $forall item <- itemsW
      <li>^{item}
  |]
compileBlock' (DefinitionList items) = do
  itemsW <- mapM (\(item, defs) -> (,) <$> compileInlines item <*> mapM compileBlocks defs) items
  pure
    [whamlet|
  <dl>
    $forall (item,defs) <- itemsW
      <dt>^{item}
      $forall def <- defs
        <dd>^{def}
  |]
compileBlock' (Figure attr caption fig) = do
  figW <- compileBlocks fig
  captionW <- compileBlocks caption
  pure
    [whamlet|
  <figure *{compileAttr attr}>
    ^{figW}
    <figcaption>^{captionW}
  |]
compileBlock' (Embed i) = do
  let lnk = embedLnk i
  lvl <- use currentLevel
  widget <- lift $ embedBody i lvl
  pure
    [whamlet|
  <div .embedded>
    ^{lnk}
    ^{widget}
|]
compileBlock' (EmbedHeader i) = do
  let lnk = embedLnk i
  lvl <- use currentLevel
  widget <- lift $ embedBody i $ lvl + 1
  titleText <- lift $ rSelectMtdt Title $ sqlId i
  title <- lift $ compileHeader (lvl + 1) [whamlet|#{fromMaybe "" titleText} ^{lnk}|]
  pure $ Wdgs.mkSection (lvl + 1) [] [] title widget
compileBlock' (Sub hd) = do
  -- Compute level shift
  rtLvl <- use hdRootLevel
  let lvl = rtLvl + hd ^. hdLevel
  -- Set header id
  let htmlId = hd ^. hdAttr . attrId
  let hdId = if T.null htmlId then Nothing else Just htmlId
  currentHeaderId .= hdId
  -- Render header content
  subC <- use subCount
  subCount .= 0
  subLoc . subOffsets %= (subC :)
  subL <- use subLoc
  oldCodeCount <- use codeCount
  codeCount .= 0
  oldCheckCount <- use checkboxCount
  checkboxCount .= 0
  contentW <- withLevel lvl $ compileBlocks $ hd ^. hdContent
  subLoc . subOffsets %= tail
  subCount .= subC + 1
  codeCount .= oldCodeCount
  checkboxCount .= oldCheckCount
  -- Render header
  editIdent <- newIdent
  entry <- use currentEntry
  hdW <- lift $ compileHead entry lvl hdId (hd ^. hdTitle) editIdent (hd ^. hdTask) (hd ^. hdChecks) subL
  openedLoc <- use openedSub
  let collapsedClass :: [Text] = ["collapsed" | not (subPrefix subL openedLoc)]
  let taskClass :: [Text] = ["task-section" | isJust (hd ^. hdTask)]
  let classes = compileAttrWithClasses (collapsedClass ++ taskClass) $ hd ^. hdAttr
  pure $ Wdgs.mkSection lvl classes [("id", editIdent)] hdW contentW
compileBlock' (Table tbl) = do
  tableW <- compileTable (tbl ^. tableHeader) (tbl ^. tableFooter) (tbl ^. tableCells)
  captionW <- compileBlocks $ tbl ^. tableCaption
  pure
    [whamlet|
  <figure *{compileAttr $ tbl ^. tableAttr}>
    <table>
      ^{tableW}
    <figcaption>
      ^{captionW}
  |]

embedLnk :: Id -> Widget
embedLnk i =
  [whamlet|
  <a href=@{EntryR $ WId i}>
    <code>
      @#{unId i}
|]

embedBody :: Id -> Int -> Handler Widget
embedBody i lvl =
  load i >>= \case
    Nothing -> pure mempty
    Just entry -> do
      case entry ^. kindData of
        LinkD link -> Link.embed lvl link
        FileD file -> File.embed lvl file
        EventD event -> Event.embed lvl event
        CalendarD cal -> Cal.embed lvl cal
        NoteD note -> embed lvl note

compileAttrWithClasses :: [Text] -> Attr -> [(Text, Text)]
compileAttrWithClasses cls attr =
  [("id", attr ^. attrId) | not (T.null $ attr ^. attrId)]
    ++ [("class", T.intercalate " " $ cls ++ (attr ^. attrClasses))]
    ++ M.toList (attr ^. attrMtdt)

compileAttr :: Attr -> [(Text, Text)]
compileAttr = compileAttrWithClasses []

compileHead :: Id -> Int -> Maybe Text -> Text -> Text -> Maybe Task -> Checks -> SubLoc -> Handler Widget
compileHead entry n hdId t edit task checks subL = do
  btm <- editButton entry (min n 5) hdId edit subL
  compileHeader n [whamlet|^{taskWidget entry subL task} #{t} ^{checksDisplay checks} ^{btm}|]

taskWidget :: Id -> SubLoc -> Maybe Task -> Widget
taskWidget _ _ Nothing = mempty
taskWidget i subL (Just task) = do
  spanId <- newIdent
  let loc = LocTask $ TaskLoc subL
  toWidget
    [julius|
    setupTask("@{NoteSubR (WId i) (WLoc loc)}", #{spanId})
  |]
  [whamlet|
  <span ##{spanId} .task-span .#{status}>
    ^{label}
|]
  where
    label = task ^. tskStatusName
    status :: Text
    status = case task ^. tskStatus of
      TaskTodo -> "task-todo"
      TaskOngoing -> "task-ongoing"
      TaskBlocked -> "task-blocked"
      TaskDone -> "task-done"
      TaskDont -> "task-dont"

compileHeader :: Int -> Widget -> Handler Widget
compileHeader 0 tit =
  pure [whamlet|<h1> ^{Wdgs.headerSymbol "●"} ^{tit}|]
compileHeader 1 tit =
  pure [whamlet|<h2> ^{Wdgs.headerSymbol "◉"} ^{tit}|]
compileHeader 2 tit =
  pure [whamlet|<h3> ^{Wdgs.headerSymbol "✿"} ^{tit}|]
compileHeader 3 tit =
  pure [whamlet|<h4> ^{Wdgs.headerSymbol "✸"} ^{tit}|]
compileHeader 4 tit =
  pure [whamlet|<h5> ^{Wdgs.headerSymbol "○"} ^{tit}|]
compileHeader 5 tit =
  pure [whamlet|<h6> ^{Wdgs.headerSymbol "◆"} ^{tit}|]
compileHeader _ tit =
  pure [whamlet|<h6> ^{Wdgs.headerSymbol "◇"} ^{tit}|]

aceRedirect :: Id -> Maybe Text -> SubLoc -> Handler Text
aceRedirect i mhdId loc = do
  render <- getUrlRenderParams
  let url = render (EntryR $ WId i) [("open", renderLoc $ LocSub loc)]
  pure $ case mhdId of
    Nothing -> url
    Just hdId -> url <> "#" <> hdId

editButton :: Id -> Int -> Maybe Text -> Text -> SubLoc -> Handler Widget
editButton entry i hdId edit subL = do
  buttonId <- newIdent
  redirUrl <- aceRedirect entry hdId subL
  js <- Ace.editOnClick buttonId edit "pandoc" link redirUrl
  pure $
    js
      >> [whamlet|
    <span ##{buttonId} .edit-header .#{"edit-header-" <> show lvl}>
      ✎
  |]
  where
    lvl :: Int
    lvl = i + 1
    link :: Route WebData
    link =
      if null (subL ^. subOffsets)
        then NoteR (WId entry)
        else NoteSubR (WId entry) $ WLoc $ LocSub subL

checksDisplay :: Checks -> Widget
checksDisplay (Checks 0 0 0 0 0) = mempty
checksDisplay (Checks todo ongoing blocked done dont) =
  [whamlet|
    <span .checks-count>
      <span class=todo-count>#{show todo}</span>/<span class=ongoing-count>#{show ongoing}</span>/<span class=blocked-count>#{show blocked}</span>/<span class=done-count>#{show done}</span>/<span class=dont-count>#{show dont}</span>
  |]

compileTable :: Int -> Int -> Array (Int, Int) Cell -> CompileM Widget
compileTable header footer cells = do
  rowsW <- mapM (\row -> compileTableRow (isHeader row) cells row) rows
  pure
    [whamlet|
  $forall row <- rowsW
    <tr>
      ^{row}
  |]
  where
    isHeader row = row <= header || row >= footer
    nrows = bounds cells ^. _2 . _2
    rows = [1 .. nrows]

compileTableRow :: Bool -> Array (Int, Int) Cell -> Int -> CompileM Widget
compileTableRow isHeader cells row =
  fmap mconcat $ forM [1 .. ncols] $ \col -> do
    let cell = cells ! (col, row)
    if cell ^. cellOrig == (col, row)
      then do
        let cattrs :: [(Text, Text)] =
              [("colspan", T.pack . show $ cell ^. cellWidth) | cell ^. cellWidth > 1]
                ++ [("rowspan", T.pack . show $ cell ^. cellHeight) | cell ^. cellHeight > 1]
        dat <- compileBlocks $ cell ^. cellData
        if isHeader
          then pure [whamlet|<th *{cattrs}>^{dat}|]
          else pure [whamlet|<td *{cattrs}>^{dat}|]
      else pure mempty
  where
    ncols = bounds cells ^. _2 . _1

compileInlines :: [Inline] -> CompileM Widget
compileInlines = fmap mconcat . mapM compileInline

compileInline :: Inline -> CompileM Widget
compileInline (Plain txt) = pure $ toWidget txt
compileInline (Styled Emph inls) = do
  inlsW <- compileInlines inls
  pure [whamlet|<em>^{inlsW}|]
compileInline (Styled Quote inls) = do
  inlsW <- compileInlines inls
  pure [whamlet|<q>^{inlsW}|]
compileInline (Styled SubScript inls) = do
  inlsW <- compileInlines inls
  pure [whamlet|<sub>^{inlsW}|]
compileInline (Styled SuperScript inls) = do
  inlsW <- compileInlines inls
  pure [whamlet|<sup>^{inlsW}|]
compileInline (Code attr txt) = pure [whamlet|<code *{compileAttr attr}>#{txt}|]
compileInline (Link attr inls tgt) = do
  inlsW <- compileInlines inls
  pure [whamlet|<a href=@{EntryR $ WId tgt} *{compileAttr attr}>^{inlsW}|]
compileInline (Cite i) = pure [whamlet|<cite><a href=@{EntryR $ WId i}>@#{unId i}|]
compileInline (PlainLink mtitle uri) = do
  let url = show uri
  title <- case mtitle of
    Just t -> compileInlines t
    Nothing -> pure [whamlet|#{url}|]
  pure [whamlet|<a href=#{url}>^{title}|]
compileInline Space = pure $ toWidget (" " :: Text)
compileInline Break = pure [whamlet|<br>|]
compileInline (DisplayMath mth) = pure . toWidget . toHtml $ "$$" <> mth <> "$$"
compileInline (InlineMath mth) = pure . toWidget . toHtml $ "\\(" <> mth <> "\\)"
compileInline (Sidenote side) = do
  note <- pushSide side
  pure [whamlet|<span .sidenote-ref>#{show note}|]
compileInline (Check ck) = do
  render <- getUrlRender
  loc <- CheckLoc <$> use subLoc <*> use checkboxCount
  entry <- use currentEntry
  let todoUrl = render $ checkImg TaskTodo
  let ongoingUrl = render $ checkImg TaskOngoing
  let blockedUrl = render $ checkImg TaskBlocked
  let doneUrl = render $ checkImg TaskDone
  let dontUrl = render $ checkImg TaskDont
  let postUrl = render $ NoteSubR (WId entry) $ WLoc $ LocCheck loc
  checkboxCount += 1
  cid <- newIdent
  pure $ do
    toWidget [julius|setupCheckbox(#{postUrl}, #{todoUrl}, #{ongoingUrl}, #{blockedUrl}, #{doneUrl}, #{dontUrl}, #{cid});|]
    [whamlet|<img ##{cid} src=@{checkImg ck} .checkBox>|]

checkImg :: TaskStatus -> Route WebData
checkImg TaskTodo = StaticR $ StaticRoute ["icons", "checkbox-todo.svg"] []
checkImg TaskOngoing = StaticR $ StaticRoute ["icons", "checkbox-ongoing.svg"] []
checkImg TaskBlocked = StaticR $ StaticRoute ["icons", "checkbox-blocked.svg"] []
checkImg TaskDone = StaticR $ StaticRoute ["icons", "checkbox-done.svg"] []
checkImg TaskDont = StaticR $ StaticRoute ["icons", "checkbox-dont.svg"] []
