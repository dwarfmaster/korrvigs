module Korrvigs.Web.Entry.Note (content) where

import Control.Lens
import Control.Monad
import Control.Monad.Loops (whileJust)
import Control.Monad.State
import Data.Array
import Data.Default
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Note
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Yesod hiding (Attr, get, (.=))

data CompileState = CState
  { _noteCounter :: Int,
    _notes :: [(Int, [Block])],
    _topLevel :: Bool
  }

makeLenses ''CompileState

instance Default CompileState where
  def = CState 1 [] True

type CompileM = State CompileState

runCompile :: CompileM Widget -> Widget
runCompile act = evalState act def

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

content :: Note -> Handler Widget
content note = do
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
    Right md -> pure $ do
      Rcs.mathjax
      runCompile $ compileBlocks $ md ^. docContent
      toWidget
        [julius|
        var syms = document.querySelectorAll('.section-symbol')
        for(let sym = 0; sym < syms.length; sym++) {
          syms[sym].addEventListener("click", function () {
            syms[sym].parentElement.parentElement.classList.toggle("collapsed")
          })
        }
      |]

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
-- TODO syntax highlighting
compileBlock' (CodeBlock attr code) =
  pure
    [whamlet|
  <div .sourceCode *{compileAttr attr}>
    <pre .sourceCode>
      #{code}
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
-- TODO deal with embedded documents
compileBlock' (Embed i) = pure [whamlet|<a href=@{EntryR $ WId i}>Embedded document|]
compileBlock' (Sub hd) = do
  contentW <- compileBlocks $ hd ^. hdContent
  pure
    [whamlet|
    <section *{compileAttr $ hd ^. hdAttr} .collapsed class=#{"level" ++ show lvl}>
      ^{compileHead (hd ^. hdLevel) (hd ^. hdTitle)}
      <div .section-content>
        ^{contentW}
  |]
  where
    lvl = hd ^. hdLevel + 1
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

compileAttr :: Attr -> [(Text, Text)]
compileAttr attr =
  [("id", attr ^. attrId) | not (T.null $ attr ^. attrId)]
    ++ (("class",) <$> attr ^. attrClasses)
    ++ M.toList (attr ^. attrMtdt)

symb :: Text -> Widget
symb s = [whamlet|<span .section-symbol>#{s}|]

compileHead :: Int -> Text -> Widget
compileHead 0 t = [whamlet|<h1> ^{symb "●"} #{t}|]
compileHead 1 t = [whamlet|<h2> ^{symb "◉"} #{t}|]
compileHead 2 t = [whamlet|<h3> ^{symb "✿"} #{t}|]
compileHead 3 t = [whamlet|<h4> ^{symb "✸"} #{t}|]
compileHead 4 t = [whamlet|<h5> ^{symb "○"} #{t}|]
compileHead 5 t = [whamlet|<h6> ^{symb "◆"} #{t}|]
compileHead _ t = [whamlet|<h6> ^{symb "◇"} #{t}|]

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
compileInline (Link attr inls target) = do
  inlsW <- compileInlines inls
  pure [whamlet|<a href=@{EntryR $ WId target} *{compileAttr attr}>^{inlsW}|]
compileInline (Cite i) = pure [whamlet|<cite><a href=@{EntryR $ WId i}>@#{unId i}|]
compileInline (PlainLink uri) =
  let url = show uri in pure [whamlet|<a href=#{url}>#{url}|]
compileInline Space = pure $ toWidget (" " :: Text)
compileInline Break = pure [whamlet|<br>|]
compileInline (DisplayMath mth) = pure . toWidget . toHtml $ "$$" <> mth <> "$$"
compileInline (InlineMath mth) = pure . toWidget . toHtml $ "\\(" <> mth <> "\\)"
compileInline (Sidenote side) = do
  note <- pushSide side
  pure [whamlet|<span .sidenote-ref>#{show note}|]
