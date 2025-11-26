module Korrvigs.Web.Entry.Note
  ( content,
    embed,
    embedContent,
    editButton,
    embedBody,
    embedLnk,
    resultWidget,
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.Loops (whileJust)
import Control.Monad.State
import Data.Array
import Data.ByteString (ByteString)
import qualified Data.Csv as Csv
import Data.Default
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Vector (Vector)
import qualified Data.Vector as V
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.File.SQL
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Note hiding (code, task)
import Korrvigs.Utils.JSON
import qualified Korrvigs.Web.Ace as Ace
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Calendar as Cal
import qualified Korrvigs.Web.Entry.Event as Event
import qualified Korrvigs.Web.Entry.File as File
import qualified Korrvigs.Web.Entry.Syndicate as Syn
import Korrvigs.Web.Public.Crypto (mkPublic)
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Search.Results
import Korrvigs.Web.Widgets (applyAttr, openIcon)
import qualified Korrvigs.Web.Widgets as Wdgs
import Opaleye hiding (min, not, null)
import Text.Blaze hiding ((!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Text.Julius hiding (js)
import Yesod hiding (Attr, get, (.=))

data CompileState = CState
  { _noteCounter :: Int,
    _notes :: [(Int, [Block])],
    _topLevel :: Bool,
    _hdRootLevel :: Int,
    _currentLevel :: Int,
    _embedded :: Bool,
    _currentEntry :: Id,
    _currentDoc :: Document,
    _subLoc :: SubLoc,
    _subCount :: Int,
    _codeCount :: Int,
    _checkboxCount :: Int,
    _openedSub :: SubLoc,
    _currentHeaderId :: Maybe Text,
    _editEnabled :: Bool,
    _replaceComputations :: Bool
  }

makeLenses ''CompileState

initCState :: Document -> Id -> CompileState
initCState doc i =
  CState 1 [] True 0 0 False i doc (SubLoc []) 0 0 0 (SubLoc []) Nothing False True

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

embed :: Int -> Note -> Handler (Widget, Checks)
embed lvl note = do
  let path = note ^. notePath
  mmd <- readNote path
  case mmd of
    Left err ->
      pure
        ( [whamlet|
        <p>
          Failed to load
          <em>#{path}
          :
        <code>
          #{err}
      |],
          def
        )
    Right md -> do
      subL <- fmap parseLoc <$> lookupGetParam "open"
      let msubL = either (const Nothing) Just =<< subL
      embedContent True True lvl msubL (note ^. noteEntry . entryName) md (md ^. docContent) (md ^. docChecks)

embedContent :: Bool -> Bool -> Int -> Maybe AnyLoc -> Id -> Document -> [Block] -> Checks -> Handler (Widget, Checks)
embedContent enableEdit repComps lvl subL i doc cnt checks = do
  let isEmbedded = lvl > 0
  let st' =
        initCState doc i
          & hdRootLevel .~ lvl
          & currentLevel .~ lvl
          & embedded .~ isEmbedded
          & editEnabled .~ enableEdit
          & replaceComputations .~ repComps
  let st = case subL of
        Just (LocSub loc) -> st' & openedSub .~ loc
        Just (LocCode loc) -> st' & openedSub .~ (loc ^. codeSub)
        _ -> st'
  markdown <- runCompile st $ compileBlocks cnt
  let w = do
        Ace.setup
        Rcs.mathjax StaticR
        [whamlet|
         $if not isEmbedded
           <p .checks-top>
             ^{checksDisplay checks}
       |]
        markdown
        unless isEmbedded $ do
          Wdgs.sectionLogic
          toWidget [julius|checkboxCleanSpans()|]
  pure (w, checks)

content :: Note -> Handler Widget
content = fmap fst . embed 0

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
  divId <- newIdent
  public <- lift isPublic
  enableEdit <- use editEnabled
  repComp <- use replaceComputations
  let edit = not public && enableEdit
  let language = fromMaybe "text" $ find Ace.isLanguage $ attr ^. attrClasses
  (cls, widget) <-
    use currentDoc >>= \doc -> case if repComp then M.lookup (attr ^. attrId) (doc ^. docComputations) else Nothing of
      Just (tp, _, res) -> do
        i <- use currentEntry
        widget <- lift $ resultWidget i (attr ^. attrId) tp res
        pure ("computation-result" :: Text, widget)
      Nothing -> lift $ ("sourceCode",) <$> Ace.preview code language
  entry <- use currentEntry
  subL <- use subLoc
  codeO <- use codeCount
  let loc = LocCode $ CodeLoc subL codeO
  hdId <- use currentHeaderId
  redirUrl <- lift $ aceRedirect entry hdId subL
  buttonId <- newIdent
  js <-
    lift $
      Ace.editOnClick
        buttonId
        divId
        language
        (NoteSubR (WId entry) $ WLoc loc)
        redirUrl
  let editW =
        if not edit
          then mempty
          else
            [whamlet|
    <div ##{buttonId} .edit-code>
      ✎
  |]
  let cdId = attr ^. attrId
  attrs <- compileAttr attr
  codeCount %= (+ 1)
  pure $ do
    when edit js
    [whamlet|
    <div ##{divId} class=#{cls} *{attrs}>
      ^{editW}
      $if not (T.null cdId)
        <a href=@{NoteNamedCodeR (WId entry) cdId} .open-code>
          ^{openIcon}
      ^{widget}
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
  attrs <- compileAttr attr
  pure
    [whamlet|
  <figure *{attrs}>
    ^{figW}
    <figcaption>^{captionW}
  |]
compileBlock' (Embed i) = do
  lnk <- lift $ embedLnk i
  lvl <- use currentLevel
  embedId <- newIdent
  (widget, checks, _) <- lift $ embedBody i lvl
  pure $ do
    propagateChecks embedId checks
    [whamlet|
  <div ##{embedId} .embedded>
    ^{lnk}
    ^{widget}
|]
compileBlock' (EmbedHeader i) = do
  lnk <- lift $ embedLnk i
  lvl <- use currentLevel
  (widget, checks, title) <- lift $ embedBody i $ lvl + 1
  sqlI <- lift $ rSelectOne (fromName pure $ sqlId i) >>= throwMaybe (KMiscError $ "Couldn't load " <> unId i)
  task <- lift $ loadTask i sqlI title
  taskW <- lift $ Wdgs.taskWidget i (SubLoc []) task
  titleW <- lift $ compileHeader (lvl + 1) [whamlet|^{taskW} #{fromMaybe "" title} ^{checksDisplay checks} ^{lnk}|]
  pure $ do
    embedId <- Wdgs.mkSection (lvl + 1) [("class", "collapsed")] [] titleW widget
    case task of
      Nothing -> propagateChecks embedId checks
      Just tk -> do
        let tkchecks =
              Checks
                (if tk ^. tskStatus == TaskTodo then 1 else 0)
                (if tk ^. tskStatus == TaskImportant then 1 else 0)
                (if tk ^. tskStatus == TaskOngoing then 1 else 0)
                (if tk ^. tskStatus == TaskBlocked then 1 else 0)
                (if tk ^. tskStatus == TaskDone then 1 else 0)
                (if tk ^. tskStatus == TaskDont then 1 else 0)
        propagateChecks embedId tkchecks
compileBlock' (Collection col nm items) = do
  dat <- lift $ loadCollection col items
  let checks = foldr (updChecks . snd) def dat
  wdg <- lift $ displayResults col True dat
  i <- use currentEntry
  rtLevel <- use hdRootLevel
  webId <- if rtLevel > 1 then newIdent else pure nm
  cl <- lift $ colWidget i webId wdg
  pure $ do
    propagateChecks webId checks
    cl
  where
    updChecks :: OptionalSQLData -> Checks -> Checks
    updChecks dat = case dat ^. optTask >>= parseStatusName of
      Just TaskTodo -> ckTodo %~ (+ 1)
      Just TaskImportant -> ckImportant %~ (+ 1)
      Just TaskOngoing -> ckOngoing %~ (+ 1)
      Just TaskBlocked -> ckBlocked %~ (+ 1)
      Just TaskDone -> ckDone %~ (+ 1)
      Just TaskDont -> ckDont %~ (+ 1)
      Nothing -> id
compileBlock' (Syndicate nm onlyNew ids) = do
  entries <- fmap catMaybes $ forM ids $ lift . load
  let syns = mapMaybe (^? _Syndicate) entries
  itemsWidget <-
    lift $
      Syn.renderItems syns $
        def
          & Syn.renderOnlyNew .~ onlyNew
          & Syn.renderShowSyndicate .~ True
  rtLevel <- use hdRootLevel
  webId <- if rtLevel > 1 then newIdent else pure nm
  pure
    [whamlet|
    <details .collection ##{webId} open="true">
      <summary>
        ##{nm}
      ^{itemsWidget}
  |]
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
  subLoc . subOffsets %= drop 1
  subCount .= subC + 1
  codeCount .= oldCodeCount
  checkboxCount .= oldCheckCount
  -- Render header
  editIdent <- newIdent
  entry <- use currentEntry
  enableEdit <- use editEnabled
  hdW <- lift $ compileHead entry lvl hdId (hd ^. hdTitle) editIdent (hd ^. hdTask) (hd ^. hdChecks) subL enableEdit
  openedLoc <- use openedSub
  let collapsedClass :: [Text] = ["collapsed" | not (subPrefix subL openedLoc)]
  let taskClass :: [Text] = ["task-section" | isJust (hd ^. hdTask)]
  classes <- compileAttrWithClasses (collapsedClass ++ taskClass) $ hd ^. hdAttr
  pure $ void $ Wdgs.mkSection lvl classes [("id", editIdent)] hdW contentW
compileBlock' (Table tbl) = do
  tableW <- compileTable (tbl ^. tableHeader) (tbl ^. tableFooter) (tbl ^. tableCells)
  captionW <- compileBlocks $ tbl ^. tableCaption
  attrs <- compileAttr $ tbl ^. tableAttr
  pure
    [whamlet|
  <figure *{attrs}>
    <table>
      ^{tableW}
    <figcaption>
      ^{captionW}
  |]

colWidget :: Id -> Text -> Widget -> Handler Widget
colWidget i nm widget = do
  pure
    [whamlet|
    <details .collection ##{nm} open="true">
      <summary>
        ##{nm}
        <a href=@{NoteColR (WId i) nm}>
          ^{openIcon}
      ^{widget}
  |]

propagateChecks :: Text -> Checks -> Widget
propagateChecks _ cks | cks == def = mempty
propagateChecks embedId cks =
  toWidget
    [julius|
    propagateChecks(#{embedId}, [#{todo}, #{important}, #{ongoing}, #{blocked}, #{done}, #{dont}])
  |]
  where
    jsInt :: Int -> RawJavascript
    jsInt = rawJS . show
    todo = jsInt $ cks ^. ckTodo
    important = jsInt $ cks ^. ckImportant
    ongoing = jsInt $ cks ^. ckOngoing
    blocked = jsInt $ cks ^. ckBlocked
    done = jsInt $ cks ^. ckDone
    dont = jsInt $ cks ^. ckDont

embedLnk :: Id -> Handler Widget
embedLnk i = do
  url <- mkPublic $ EntryR $ WId i
  pure
    [whamlet|
  <a href=@{url}>
    <code>
      @#{unId i}
|]

embedBody :: Id -> Int -> Handler (Widget, Checks, Maybe Text)
embedBody i lvl =
  load i >>= \case
    Nothing -> pure (mempty, def, Nothing)
    Just entry -> do
      let title = entry ^. entryTitle
      case entry ^. entryKindData of
        FileD file -> (,def,title) <$> File.embed lvl file
        EventD event -> (,def,title) <$> Event.embed lvl event
        CalendarD cal -> (,def,title) <$> Cal.embed lvl cal
        NoteD note -> (\(w, c) -> (w, c, title)) <$> embed lvl note
        SyndicateD syn -> (,def,title) <$> Syn.embed lvl syn

compileAttrWithClasses :: [Text] -> Attr -> CompileM [(Text, Text)]
compileAttrWithClasses cls attr = do
  rtLevel <- use hdRootLevel
  pure $
    [("id", attr ^. attrId) | not (T.null $ attr ^. attrId) && rtLevel == 1]
      ++ [("class", T.intercalate " " $ cls ++ (attr ^. attrClasses))]
      ++ M.toList (attr ^. attrMtdt)

compileAttr :: Attr -> CompileM [(Text, Text)]
compileAttr = compileAttrWithClasses []

compileAttr' :: Attr -> Html -> Html
compileAttr' (MkAttr i clss misc) = applyId . applyClasses . applyMisc
  where
    applyId = if T.null i then id else applyAttr $ Attr.id $ textValue i
    applyClasses = if null clss then id else applyAttr $ Attr.class_ $ textValue $ T.intercalate " " clss
    applyMisc = appEndo $ foldMap (\(k, v) -> Endo $ applyAttr $ customAttribute (textTag k) $ textValue v) $ M.toList misc

compileHead :: Id -> Int -> Maybe Text -> Text -> Text -> Maybe Task -> Checks -> SubLoc -> Bool -> Handler Widget
compileHead entry n hdId t edit task checks subL enableEdit = do
  btm <- editButton entry (min n 5) hdId edit subL
  tsk <- Wdgs.taskWidget entry subL task
  let editBtm = if enableEdit then btm else mempty
  let openBtm = case hdId of
        Just i ->
          [whamlet|
           <a href=@{NoteNamedSubR (WId entry) i}>
             ^{openIcon}
         |]
        Nothing -> mempty
  compileHeader n [whamlet|^{tsk} #{t} ^{openBtm} ^{checksDisplay checks} ^{editBtm}|]

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
  public <- isPublic
  buttonId <- newIdent
  redirUrl <- aceRedirect entry hdId subL
  js <- Ace.editOnClick buttonId edit "pandoc" link redirUrl
  pure $
    if public
      then mempty
      else do
        js
        [whamlet|
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
checksDisplay (Checks todo important ongoing blocked done dont) =
  [whamlet|
    <span .checks-count>
      <span class=important-count>#{show important}</span>/<span class=todo-count>#{show todo}</span>/<span class=ongoing-count>#{show ongoing}</span>/<span class=blocked-count>#{show blocked}</span>/<span class=done-count>#{show done}</span>/<span class=dont-count>#{show dont}</span>
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
  lift isPublic >>= \case
    False -> pure (applyAttr (Attr.href link) $ compileAttr' attr $ Html.a inlsH, inlsW)
    True -> do
      isFile <- lift $ rSelectOne $ do
        file <- selectTable filesTable
        nm <- nameFor $ file ^. sqlFileId
        where_ $ nm .== sqlId tgt
        pure ()
      case isFile of
        Nothing -> pure (inlsH, inlsW)
        Just () -> do
          route <- lift $ mkPublic $ EntryDownloadR $ WId tgt
          let pubLink = textValue $ render route
          pure (applyAttr (Attr.href pubLink) $ compileAttr' attr $ Html.a inlsH, inlsW)
compileInline (Cite i) = do
  render <- getUrlRender
  let link = textValue $ render $ EntryR $ WId i
  lift isPublic >>= \case
    False -> pure (applyAttr (Attr.href link) $ Html.a $ toMarkup $ unId i, mempty)
    True -> pure (toMarkup $ unId i, mempty)
compileInline (MtdtLink mtitle mtdt) = do
  i <- use currentEntry
  mtdtVal <- lift $ rSelectOne $ do
    m <- selectTable entriesMetadataTable
    sqlI <- fromName pure $ sqlId i
    where_ $ m ^. sqlEntry .== sqlI
    where_ $ m ^. sqlKey .== sqlStrictText mtdt
    pure $ sqlJsonToText $ toNullable $ m ^. sqlValue
  case mtdtVal of
    Just (Just url) -> plainLink mtitle url
    _ -> pure (mempty, mempty)
compileInline (PlainLink mtitle uri) =
  plainLink mtitle $ T.pack $ show uri
compileInline Space = pure (toMarkup (" " :: Text), mempty)
compileInline Break = pure (Html.br, mempty)
compileInline (DisplayMath mth) = pure (toMarkup $ "$$" <> mth <> "$$", mempty)
compileInline (InlineMath mth) = pure (toMarkup $ "\\(" <> mth <> "\\)", mempty)
compileInline (Sidenote side) = do
  note <- pushSide side
  pure (applyAttr (Attr.class_ "sidenote-ref") $ Html.span $ toMarkup $ show note, mempty)
compileInline (Check ck) = do
  loc <- CheckLoc <$> use subLoc <*> use checkboxCount
  entry <- use currentEntry
  checkboxCount += 1
  (h, w, _) <- lift $ Wdgs.checkBox ck $ NoteSubR (WId entry) $ WLoc $ LocCheck loc
  public <- lift isPublic
  pure (h, if public then mempty else w)

plainLink :: Maybe [Inline] -> Text -> CompileM (Html, Widget)
plainLink mtitle url = do
  (titleH, titleW) <- case mtitle of
    Just t -> compileInlines' t
    Nothing -> pure (toMarkup url, mempty)
  pure (applyAttr (Attr.href $ textValue url) $ Html.a titleH, titleW)

resultWidget :: Id -> Text -> RunnableType -> RunnableResult -> Handler Widget
resultWidget i cmp ScalarImage _ =
  pure [whamlet|<img src=@{EntryComputeR (WId i) cmp} .computation-img>|]
resultWidget i cmp ScalarGraphic _ =
  pure [whamlet|<img src=@{EntryComputeR (WId i) cmp} .computation-img>|]
resultWidget i cmp VectorGraphic _ =
  pure [whamlet|<img src=@{EntryComputeR (WId i) cmp} .computation-img>|]
resultWidget i cmp ArbitraryJson _ =
  pure $ do
    Rcs.andypfJsonViewer StaticR
    [whamlet|
      <andypf-json-viewer
        indent="2"
        expanded="false"
        theme="default-dark"
        show-data-types="true"
        show-toolbar="false"
        expand-icon-type="arrow"
        show-copy="true"
        show-size="true"
        data=@{EntryComputeR (WId i) cmp}>
    |]
resultWidget _ _ ArbitraryText (ResultText txt) =
  pure [whamlet|<pre>#{txt}</pre>|]
resultWidget _ _ ArbitraryText _ = pure mempty
resultWidget _ _ TabularCsv (ResultText txt) =
  case Csv.decode Csv.NoHeader (LEnc.encodeUtf8 $ LT.fromStrict txt) of
    Left _ -> pure mempty
    Right (csv :: Vector (Vector ByteString)) ->
      pure
        [whamlet|
        <table>
          <thead>
            $forall line <- V.take 1 csv
              <tr>
                $forall item <- line
                  <th>
                    #{Enc.decodeUtf8Lenient item}
          <tbody>
            $forall line <- V.drop 1 csv
              <tr>
                $forall item <- line
                  <td>
                    #{Enc.decodeUtf8Lenient item}
      |]
resultWidget _ _ TabularCsv _ = pure mempty
