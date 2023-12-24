module Korrvigs.Web.Entry.Pandoc where

import Control.Monad (forM_, when)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State.Lazy (StateT, execStateT, modify, runStateT)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Web.Backend
import Skylighting (defaultSyntaxMap)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting
import Text.Pandoc.Shared (stringify)
import Yesod hiding (Header, Null)

-- The structure we are compiling the pandoc document to. It allows recovering
-- a structural description of the document along headers. This structure is
-- then compiled to a flat widget.
data WidgetTreeItem
  = WTIBlock Widget
  | WTISection Text Widget WidgetTree
  | WTISub (Widget -> Widget) WidgetTree

newtype WidgetTree = WidgetTree [WidgetTreeItem]

data WidgetTreeZipper = MkZipper
  { zipper_level :: Int,
    zipper_parent :: Maybe (Text, Widget, WidgetTreeZipper),
    zipper_left :: [WidgetTreeItem],
    zipper_right :: [WidgetTreeItem]
  }

mkZipper :: WidgetTreeZipper
mkZipper = MkZipper (-1) Nothing [] []

zipperPushSection :: Int -> Text -> Widget -> WidgetTreeZipper -> WidgetTreeZipper
zipperPushSection lvl i title zipper =
  MkZipper
    { zipper_level = lvl,
      zipper_parent = Just (i, title, zipper),
      zipper_left = [],
      zipper_right = []
    }

zipperPush :: WidgetTreeItem -> WidgetTreeZipper -> WidgetTreeZipper
zipperPush item (MkZipper lvl parent left right) =
  MkZipper
    { zipper_level = lvl,
      zipper_parent = parent,
      zipper_left = item : left,
      zipper_right = right
    }

zipperPopSection :: WidgetTreeZipper -> WidgetTreeZipper
zipperPopSection zipper =
  case zipper_parent zipper of
    Nothing -> zipper
    Just (i, title, parent) ->
      zipperPush
        ( WTISection
            i
            title
            ( WidgetTree $
                reverse (zipper_left zipper)
                  ++ zipper_right zipper
            )
        )
        parent

unzipTree :: WidgetTreeZipper -> WidgetTree
unzipTree zipper | isJust (zipper_parent zipper) = unzipTree $ zipperPopSection zipper
unzipTree zipper = WidgetTree $ reverse (zipper_left zipper) ++ zipper_right zipper

zipperNewSection :: Int -> Text -> Widget -> WidgetTreeZipper -> WidgetTreeZipper
zipperNewSection lvl i title zipper
  | zipper_level zipper < lvl =
      zipperPushSection lvl i title zipper
zipperNewSection lvl i title zipper =
  zipperNewSection lvl i title $ zipperPopSection zipper

type ZipperM =
  ReaderT
    (Text -> Maybe Widget)
    (StateT WidgetTreeZipper Handler)

pushBlock :: Widget -> ZipperM ()
pushBlock = lift . modify . zipperPush . WTIBlock

newSection :: Int -> Text -> Widget -> ZipperM ()
newSection lvl i title = lift $ modify $ zipperNewSection lvl i title

transform :: (Widget -> Widget) -> ZipperM a -> ZipperM a
transform f act = do
  state <- ask
  (a, tree) <- lift . lift $ runStateT (runReaderT act state) mkZipper
  lift $ modify $ zipperPush $ WTISub f $ unzipTree tree
  pure a

runZipperM :: (Text -> Maybe Widget) -> ZipperM a -> Handler WidgetTree
runZipperM state m = unzipTree <$> execStateT (runReaderT m state) mkZipper

rawHandler :: ZipperM (Text -> Maybe Widget)
rawHandler = ask

mapW :: (Monad m, Monoid o) => (a -> m o) -> [a] -> m o
mapW f lst = mconcat <$> mapM f lst

-- Compiling the structure to a Widget
itemsToWidget :: Int -> WidgetTreeItem -> Widget
itemsToWidget _ (WTIBlock widget) = widget
itemsToWidget lvl (WTISection i title content) =
  let cls = "level" ++ show (lvl + 1)
   in [whamlet|
    <section id=#{i} class=#{cls}>
      ^{mkHead lvl title}
      ^{treeToWidget (lvl + 1) content}
  |]
  where
    mkHead :: Int -> Widget -> Widget
    mkHead 0 t = [whamlet|<h1> ^{t}|]
    mkHead 1 t = [whamlet|<h2> ^{t}|]
    mkHead 2 t = [whamlet|<h3> ^{t}|]
    mkHead 3 t = [whamlet|<h4> ^{t}|]
    mkHead 4 t = [whamlet|<h5> ^{t}|]
    mkHead 5 t = [whamlet|<h6> ^{t}|]
    mkHead _ t = [whamlet|<h6> ^{t}|]
itemsToWidget _ (WTISub f sub) = f $ treeToWidget 0 sub

treeToWidget :: Int -> WidgetTree -> Widget
treeToWidget lvl (WidgetTree items) = mapW (itemsToWidget lvl) items

renderPandoc :: (Text -> Maybe Widget) -> Pandoc -> Handler Widget
renderPandoc handler (Pandoc _ blocks) =
  treeToWidget 0 <$> runZipperM handler (forM_ blocks renderBlock)

renderItem :: [Block] -> ZipperM ()
renderItem blks =
  transform
    ( \blk ->
        [whamlet|
    <li>
      ^{blk}
  |]
    )
    $ forM_ blks renderBlock

-- Compiling the pandoc document to a WidgetTree
renderBlock :: Block -> ZipperM ()
renderBlock (Plain inlines) = do
  content <- mapM renderInline inlines
  forM_ content pushBlock
renderBlock (Para inlines) = do
  content <- mapW renderInline inlines
  pushBlock
    [whamlet|
    <p> ^{content}
  |]
renderBlock (LineBlock lns) = do
  content <- mapM (mapW renderInline) lns
  pushBlock
    [whamlet|
    <p .nobr>
      $forall line <- content
        ^{line}<br>
  |]
renderBlock (CodeBlock attrs code) = do
  let coloured = highlight defaultSyntaxMap formatHtmlBlock attrs code
  pushBlock $ case coloured of
    Right html -> toWidget html
    Left err ->
      [whamlet|
        <div .sourceCode title=#{err}>
          <pre .sourceCode>
            #{code}
      |]
renderBlock (RawBlock (Format "html") raw) =
  pushBlock $ toWidget $ preEscapedToMarkup raw
renderBlock (RawBlock (Format "widget") raw) = do
  handler <- rawHandler
  let blk = handler raw
  forM_ blk pushBlock
renderBlock (RawBlock _ _) = pure ()
renderBlock (BlockQuote blocks) =
  transform
    ( \content ->
        [whamlet|
    <blockquote>
      ^{content}
  |]
    )
    $ forM_ blocks renderBlock
renderBlock (OrderedList _ items) =
  transform
    ( \its ->
        [whamlet|
      <ol>
        ^{its}
    |]
    )
    $ forM_ items renderItem
renderBlock (BulletList items) =
  transform
    ( \its ->
        [whamlet|
      <ul>
        ^{its}
    |]
    )
    $ forM_ items renderItem
renderBlock (DefinitionList defs) =
  transform
    ( \content ->
        [whamlet|
      <dl>
        ^{content}
    |]
    )
    $ forM_ defs renderDef
  where
    renderDef :: ([Inline], [[Block]]) -> ZipperM ()
    renderDef (defined, definitions) = do
      dt <- mapW renderInline defined
      transform
        ( \content ->
            [whamlet|
          <dt> ^{dt}
          ^{content}
        |]
        )
        $ forM_ definitions renderDefContent
    renderDefContent :: [Block] -> ZipperM ()
    renderDefContent blocks =
      transform
        ( \content ->
            [whamlet|
          <dd>
            ^{content}
        |]
        )
        $ forM_ blocks renderBlock
renderBlock (Header lvl (i, _, _) title) = do
  content <- mapW renderInline title
  newSection lvl i content
renderBlock HorizontalRule = pushBlock [whamlet|<hr>|]
renderBlock (Table _ (Caption _ caption) _ (TableHead _ hd) body (TableFoot _ foot)) =
  transform (\w -> [whamlet|<table> ^{w}|]) $ do
    when (caption /= []) $
      transform (\w -> [whamlet| <caption> ^{w}|]) $
        forM_ caption renderBlock
    forM_ hd $ renderRow renderHCell
    forM_ body $ \(TableBody _ _ bodyHd bodyContent) -> do
      forM_ bodyHd $ renderRow renderHCell
      forM_ bodyContent $ renderRow renderCell
    forM_ foot $ renderRow renderHCell
  where
    mkIf :: Bool -> [a] -> [a]
    mkIf True l = l
    mkIf False _ = []
    renderAlign :: Alignment -> Text
    renderAlign AlignDefault = ""
    renderAlign AlignLeft = "start"
    renderAlign AlignRight = "end"
    renderAlign AlignCenter = "center"
    cellAttrs :: Cell -> [(Text, Text)]
    cellAttrs (Cell (i, _, _) ali (RowSpan rowSpan) (ColSpan colSpan) _) =
      mkIf (colSpan > 1) [("colspan", T.pack $ show colSpan)]
        <> mkIf (rowSpan > 1) [("rowspan", T.pack $ show rowSpan)]
        <> mkIf (i /= "") [("id", i)]
        <> mkIf (ali /= AlignDefault) [("style", "text-align: " <> renderAlign ali)]
    renderCell :: Cell -> ZipperM ()
    renderCell cell@(Cell _ _ _ _ content) =
      transform
        ( \w ->
            [whamlet|
        <td *{cellAttrs cell}>
          ^{w}
      |]
        )
        $ forM_ content renderBlock
    renderHCell :: Cell -> ZipperM ()
    renderHCell cell@(Cell _ _ _ _ content) =
      transform
        ( \w ->
            [whamlet|
        <th *{cellAttrs cell}>
          ^{w}
      |]
        )
        $ forM_ content renderBlock
    renderRow :: (Cell -> ZipperM ()) -> Row -> ZipperM ()
    renderRow render (Row _ cells) =
      transform (\w -> [whamlet| <tr> ^{w}|]) $ forM_ cells render
renderBlock (Div (i, _, _) blks) =
  transform
    ( \wdgt ->
        [whamlet|
      <div id=#{i}>
        ^{wdgt}
    |]
    )
    $ forM_ blks renderBlock
renderBlock Null = pure ()

wrapI :: (Widget -> Widget) -> [Inline] -> ZipperM Widget
wrapI f c = do
  content <- mapW renderInline c
  pure $ f content

renderInline :: Inline -> ZipperM Widget
renderInline (Str txt) = pure $ toWidget txt
renderInline (Emph c) = wrapI (\w -> [whamlet|<em> ^{w}|]) c
renderInline (Underline c) = wrapI (\w -> [whamlet|<u> ^{w}|]) c
renderInline (Strong c) = wrapI (\w -> [whamlet|<strong> ^{w}|]) c
renderInline (Strikeout c) = wrapI (\w -> [whamlet|<s> ^{w}|]) c
renderInline (Superscript c) = wrapI (\w -> [whamlet|<sup> ^{w}|]) c
renderInline (Subscript c) = wrapI (\w -> [whamlet|<sub> ^{w}|]) c
renderInline (SmallCaps c) = wrapI (\w -> [whamlet|<span style="font-variant: small-caps"> ^{w}|]) c
renderInline (Quoted _ c) = wrapI (\w -> [whamlet|<q> ^{w}|]) c
renderInline (Cite _ c) =
  -- TODO better handling of citation, right now nothing is done and the key are simply included inline
  mapW renderInline c
renderInline (Code (i, _, _) c) = pure [whamlet|<code id=#{i}> #{c}|]
renderInline Space = pure $ toWidget (" " :: Text)
renderInline SoftBreak = pure $ toWidget (" " :: Text)
renderInline LineBreak = pure [whamlet|<br>|]
renderInline (Math mode math) = pure $ do
  addScriptRemoteAttrs
    "https://polyfill.io/v3/polyfill.min.js?features=es6"
    [("integrity", "sha384-WSLBwI+Q8tqRHaC+f1sjS/FVv5cWp7VAfrGB17HLfZlXhbp5F/RPVP7bYVHtiAWE"), ("crossorigin", "anonymous")]
  addScriptRemoteAttrs
    "https://cdn.jsdelivr.net/npm/mathjax@3.0.1/es5/tex-mml-chtml.js"
    [("id", "MathJax-Script"), ("integrity", "sha384-/1zmJ1mBdfKIOnwPxpdG6yaRrxP6qu3eVYm0cz2nOx+AcL4d3AqEFrwcqGZVVroG"), ("crossorigin", "anonymous")]
  case mode of
    DisplayMath -> toWidget $ "$$" <> math <> "$$"
    InlineMath -> toWidget $ "\\(" <> math <> "\\)"
renderInline (Link _ c (target, title)) =
  wrapI (\w -> [whamlet|<a href=#{target} title=#{title}> ^{w}|]) c
renderInline (Image _ c (target, _)) =
  pure [whamlet|<img src=#{target} alt=#{stringify c}>|]
renderInline (Note _) = pure $ pure () -- TODO
renderInline (Span (i, _, _) c) = wrapI (\w -> [whamlet|<span id=#{i}> ^{w}|]) c
renderInline (RawInline (Format "html") raw) =
  pure $ toWidget $ preEscapedToMarkup raw
renderInline (RawInline _ _) = pure $ pure ()
