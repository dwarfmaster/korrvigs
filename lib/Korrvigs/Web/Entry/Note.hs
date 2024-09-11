module Korrvigs.Web.Entry.Note (content) where

import Control.Lens
import Control.Monad
import Data.Array
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Note
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod hiding (Attr)

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
      compileBlocks $ md ^. docContent
      toWidget
        [julius|
        var syms = document.querySelectorAll('.section-symbol')
        for(let sym = 0; sym < syms.length; sym++) {
          syms[sym].addEventListener("click", function () {
            syms[sym].parentElement.parentElement.classList.toggle("collapsed")
          })
        }
      |]

compileBlocks :: [Block] -> Widget
compileBlocks = mapM_ compileBlock

compileBlock :: Block -> Widget
compileBlock (Para inls) = [whamlet|<p>^{compileInlines inls}|]
compileBlock (LineBlock lns) =
  [whamlet|
  <p>
    $forall line <- lns
      ^{compileInlines line}
      <hr>
  |]
-- TODO syntax highlighting
compileBlock (CodeBlock attr code) =
  [whamlet|
  <div .sourceCode *{compileAttr attr}>
    <pre .sourceCode>
      #{code}
  |]
compileBlock (BlockQuote bks) = [whamlet|<blockquote>^{compileBlocks bks}|]
compileBlock (OrderedList items) =
  [whamlet|
  <ol>
    $forall item <- items
      <li>^{compileBlocks item}
  |]
compileBlock (BulletList items) =
  [whamlet|
  <ul>
    $forall item <- items
      <li>^{compileBlocks item}
  |]
compileBlock (DefinitionList items) =
  [whamlet|
  <dl>
    $forall (item,defs) <- items
      <dt>^{compileInlines item}
      $forall def <- defs
        <dd>^{compileBlocks def}
  |]
compileBlock (Figure attr caption fig) =
  [whamlet|
  <figure *{compileAttr attr}>
    ^{compileBlocks fig}
    <figcaption>^{compileBlocks caption}
  |]
-- TODO deal with embedded documents
compileBlock (Embed i) = [whamlet|<a href=@{EntryR $ WId i}>Embedded document|]
compileBlock (Sub hd) =
  [whamlet|
    <section *{compileAttr $ hd ^. hdAttr} .collapsed class=#{"level" ++ show lvl}>
      ^{compileHead (hd ^. hdLevel) (hd ^. hdTitle)}
      <div .section-content>
        ^{compileBlocks $ hd ^. hdContent}
  |]
  where
    lvl = hd ^. hdLevel + 1
compileBlock (Table tbl) =
  [whamlet|
  <figure *{compileAttr $ tbl ^. tableAttr}>
    <table>
      ^{compileTable (tbl ^. tableHeader) (tbl ^. tableFooter) (tbl ^. tableCells)}
    <figcaption>
      ^{compileBlocks $ tbl ^. tableCaption}
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

compileTable :: Int -> Int -> Array (Int, Int) Cell -> Widget
compileTable header footer cells =
  [whamlet|
  $forall row <- rows
    <tr>
      ^{compileTableRow (isHeader row) cells row}
  |]
  where
    isHeader row = row <= header || row >= footer
    nrows = bounds cells ^. _2 . _2
    rows = [1 .. nrows]

compileTableRow :: Bool -> Array (Int, Int) Cell -> Int -> Widget
compileTableRow isHeader cells row =
  forM_ [1 .. ncols] $ \col -> do
    let cell = cells ! (col, row)
    when (cell ^. cellOrig == (col, row)) $ do
      let cattrs :: [(Text, Text)] =
            [("colspan", T.pack . show $ cell ^. cellWidth) | cell ^. cellWidth > 1]
              ++ [("rowspan", T.pack . show $ cell ^. cellHeight) | cell ^. cellHeight > 1]
      let dat = compileBlocks $ cell ^. cellData
      if isHeader
        then [whamlet|<th *{cattrs}>^{dat}|]
        else [whamlet|<td *{cattrs}>^{dat}|]
  where
    ncols = bounds cells ^. _2 . _1

compileInlines :: [Inline] -> Widget
compileInlines = mapM_ compileInline

compileInline :: Inline -> Widget
compileInline (Plain txt) = toWidget txt
compileInline (Styled Emph inls) = [whamlet|<em>^{compileInlines inls}|]
compileInline (Styled Quote inls) = [whamlet|<q>^{compileInlines inls}|]
compileInline (Styled SubScript inls) = [whamlet|<sub>^{compileInlines inls}|]
compileInline (Styled SuperScript inls) = [whamlet|<sup>^{compileInlines inls}|]
compileInline (Code attr txt) = [whamlet|<code *{compileAttr attr}>#{txt}|]
compileInline (Link attr inls target) =
  [whamlet|<a href=@{EntryR $ WId target} *{compileAttr attr}>^{compileInlines inls}|]
compileInline (Cite i) = [whamlet|<cite><a href=@{EntryR $ WId i}>@#{unId i}|]
compileInline (PlainLink uri) =
  let url = show uri in [whamlet|<a href=#{url}>#{url}|]
compileInline Space = toWidget (" " :: Text)
compileInline Break = [whamlet|<br>|]
compileInline (DisplayMath mth) = undefined
compileInline (InlineMath mth) = undefined
compileInline (Sidenote side) = undefined
