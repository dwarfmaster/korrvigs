module Korrvigs.Note.Helpers where

import Control.Arrow ((***))
import Control.Lens
import qualified Data.Array as A
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Korrvigs.Entry.Ident (unId)
import Korrvigs.Note.AST
import Network.URI

inlineToText :: Inline -> Builder
inlineToText (Plain txt) = fromText txt
inlineToText (Styled _ inls) = inlinesToText inls
inlineToText (Code _ txt) = "`" <> fromText txt <> "`"
inlineToText (Link _ inls _) = inlinesToText inls
inlineToText (Cite i) = "@" <> fromText (unId i)
inlineToText (PlainLink uri) = fromString $ uriToString id uri ""
inlineToText Space = " "
inlineToText Break = "\n"
inlineToText (DisplayMath txt) = "$" <> fromText txt <> "$"
inlineToText (InlineMath txt) = "$" <> fromText txt <> "$"
inlineToText (Sidenote note) = "[" <> blocksToText note <> "]"

inlinesToText :: [Inline] -> Builder
inlinesToText = mconcat . fmap inlineToText

renderInlines :: [Inline] -> Text
renderInlines = toStrict . toLazyText . inlinesToText

blockToText :: Block -> Builder
blockToText (Para inls) = inlinesToText inls <> "\n"
blockToText (LineBlock inls) = mconcat $ intersperse "\n" $ inlinesToText <$> inls
blockToText (CodeBlock _ code) = fromText code <> "\n"
blockToText (BlockQuote quote) = blocksToText quote
blockToText (OrderedList lst) = mconcat $ blocksToText <$> lst
blockToText (BulletList lst) = mconcat $ blocksToText <$> lst
blockToText (DefinitionList lst) =
  mconcat $ uncurry (<>) . (inlinesToText *** mconcat . fmap blocksToText) <$> lst
blockToText (Figure _ caption _) = blocksToText caption
blockToText (Embed _) = mempty
blockToText (Sub hd) =
  fromText (hd ^. hdTitle) <> "\n" <> blocksToText (hd ^. hdContent)
blockToText (Table tbl) =
  mconcat . fmap (blocksToText . view cellData) . A.elems $ tbl ^. tableCells

blocksToText :: [Block] -> Builder
blocksToText = mconcat . (++ ["\n"]) . intersperse "\n" . fmap blockToText

docToText :: Document -> Builder
docToText doc = fromText (doc ^. docTitle) <> "\n" <> blocksToText (doc ^. docContent)

renderDocument :: Document -> Text
renderDocument = toStrict . toLazyText . docToText
