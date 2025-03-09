module Korrvigs.Web.Entry.Note (content, embed, editButton) where

import Control.Lens
import Control.Monad
import Control.Monad.Loops (whileJust)
import Control.Monad.State
import Data.Array
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Task
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
import Text.Blaze hiding ((!))
import qualified Text.Blaze as Blz
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
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
  pure $ Wdgs.mkSection (lvl + 1) [("class", "collapsed")] [] title widget
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

applyAttr :: Attribute -> Html -> Html
applyAttr attr html = html Blz.! attr

compileAttr' :: Attr -> Html -> Html
compileAttr' (MkAttr i clss misc) = applyId . applyClasses . applyMisc
  where
    applyId = if T.null i then id else applyAttr $ Attr.id $ textValue i
    applyClasses = if null clss then id else applyAttr $ Attr.class_ $ textValue $ T.intercalate " " clss
    applyMisc = appEndo $ foldMap (\(k, v) -> Endo $ applyAttr $ customAttribute (textTag k) $ textValue v) $ M.toList misc

compileHead :: Id -> Int -> Maybe Text -> Text -> Text -> Maybe Task -> Checks -> SubLoc -> Handler Widget
compileHead entry n hdId t edit task checks subL = do
  btm <- editButton entry (min n 5) hdId edit subL
  tsk <- Wdgs.taskWidget entry subL task
  compileHeader n [whamlet|^{tsk} #{t} ^{checksDisplay checks} ^{btm}|]

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
compileInlines inls = do
  (c, w) <- compileInlines' inls
  pure $ do
    w
    toWidget c

compileInlines' :: [Inline] -> CompileM (Html, Widget)
compileInlines' = foldM (\w i -> sidecat w <$> compileInline i) mempty
  where
    sidecat :: (Html, Widget) -> (Html, Widget) -> (Html, Widget)
    sidecat (c1, w1) (c2, w2) = (c1 <> c2, w1 <> w2)

compileInline :: Inline -> CompileM (Html, Widget)
compileInline (Plain txt) = pure (toMarkup txt, mempty)
compileInline (Styled Emph inls) = do
  (inlsH, inlsW) <- compileInlines' inls
  pure (Html.em inlsH, inlsW)
compileInline (Styled Quote inls) = do
  (inlsH, inlsW) <- compileInlines' inls
  pure (Html.q inlsH, inlsW)
compileInline (Styled SubScript inls) = do
  (inlsH, inlsW) <- compileInlines' inls
  pure (Html.sub inlsH, inlsW)
compileInline (Styled SuperScript inls) = do
  (inlsH, inlsW) <- compileInlines' inls
  pure (Html.sup inlsH, inlsW)
compileInline (Code attr txt) =
  pure (compileAttr' attr $ Html.code $ toMarkup txt, mempty)
compileInline (Link attr inls tgt) = do
  (inlsH, inlsW) <- compileInlines' inls
  render <- getUrlRender
  let link = textValue $ render $ EntryR $ WId tgt
  pure (applyAttr (Attr.href link) $ compileAttr' attr $ Html.a inlsH, inlsW)
compileInline (Cite i) = do
  render <- getUrlRender
  let link = textValue $ render $ EntryR $ WId i
  pure (applyAttr (Attr.href link) $ Html.a $ toMarkup $ unId i, mempty)
compileInline (PlainLink mtitle uri) = do
  let url = T.pack $ show uri
  (titleH, titleW) <- case mtitle of
    Just t -> compileInlines' t
    Nothing -> pure (toMarkup url, mempty)
  pure (applyAttr (Attr.href $ textValue url) $ Html.a titleH, titleW)
compileInline Space = pure (toMarkup (" " :: Text), mempty)
compileInline Break = pure (Html.br, mempty)
compileInline (DisplayMath mth) = pure (toMarkup $ "$$" <> mth <> "$$", mempty)
compileInline (InlineMath mth) = pure (toMarkup $ "\\(" <> mth <> "\\)", mempty)
compileInline (Sidenote side) = do
  note <- pushSide side
  pure (applyAttr (Attr.class_ "sidenote-ref") $ Html.span $ toMarkup $ show note, mempty)
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
  let w = toWidget [julius|setupCheckbox(#{postUrl}, #{todoUrl}, #{ongoingUrl}, #{blockedUrl}, #{doneUrl}, #{dontUrl}, #{cid});|]
  let h =
        applyAttr (Attr.id $ textValue cid) $
          applyAttr (Attr.src $ textValue $ render $ checkImg ck) $
            applyAttr (Attr.class_ "checkBox") Html.img
  pure (h, w)

checkImg :: TaskStatus -> Route WebData
checkImg TaskTodo = StaticR $ StaticRoute ["icons", "checkbox-todo.svg"] []
checkImg TaskOngoing = StaticR $ StaticRoute ["icons", "checkbox-ongoing.svg"] []
checkImg TaskBlocked = StaticR $ StaticRoute ["icons", "checkbox-blocked.svg"] []
checkImg TaskDone = StaticR $ StaticRoute ["icons", "checkbox-done.svg"] []
checkImg TaskDont = StaticR $ StaticRoute ["icons", "checkbox-dont.svg"] []
