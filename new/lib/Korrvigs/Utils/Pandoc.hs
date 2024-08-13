module Korrvigs.Utils.Pandoc where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Text.Pandoc

pdInlineToText :: Inline -> Builder
pdInlineToText (Str txt) = fromText txt
pdInlineToText (Emph inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Underline inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Strong inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Strikeout inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Superscript inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Subscript inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (SmallCaps inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Quoted _ inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Cite cites _) =
  mconcat . intersperse (fromText ";") $ map (\(Citation i _ _ _ _ _) -> "@" <> fromText i) cites
pdInlineToText (Code _ txt) = fromText txt
pdInlineToText Space = " "
pdInlineToText SoftBreak = " "
pdInlineToText LineBreak = " "
pdInlineToText (Math DisplayMath mth) = "$$" <> fromText mth <> "$$"
pdInlineToText (Math InlineMath mth) = "$" <> fromText mth <> "$"
pdInlineToText (RawInline _ _) = mempty
pdInlineToText (Link _ caption _) = mconcat $ pdInlineToText <$> caption
pdInlineToText (Image _ caption _) = mconcat $ pdInlineToText <$> caption
pdInlineToText (Note _) = mempty
pdInlineToText (Span _ inls) = mconcat $ pdInlineToText <$> inls

pdBlockToText :: Block -> Builder
pdBlockToText (Plain inls) = mconcat $ pdInlineToText <$> inls
pdBlockToText (Para inls) = mconcat $ pdInlineToText <$> inls
pdBlockToText (LineBlock inls) = mconcat $ intersperse " " $ mconcat . fmap pdInlineToText <$> inls
pdBlockToText (CodeBlock _ txt) = fromText txt
pdBlockToText (RawBlock _ txt) = fromText txt
pdBlockToText (BlockQuote bks) = pdBlocksToText bks
pdBlockToText (OrderedList _ bks) = mconcat $ intersperse " " $ pdBlocksToText <$> bks
pdBlockToText (BulletList bks) = mconcat $ intersperse " " $ pdBlocksToText <$> bks
pdBlockToText (DefinitionList lst) = mconcat $ intersperse " " $ render <$> lst
  where
    render (df, bks) = mconcat $ (pdInlineToText <$> df) <> [" "] <> intersperse " " (pdBlocksToText <$> bks)
pdBlockToText (Header _ _ inls) = mconcat $ pdInlineToText <$> inls
pdBlockToText (Div _ bks) = pdBlocksToText bks
pdBlockToText _ = mempty

pdBlocksToText :: [Block] -> Builder
pdBlocksToText bks = mconcat . intersperse " " $ pdBlockToText <$> bks

pdBlocksToRenderedText :: [Block] -> Text
pdBlocksToRenderedText = toStrict . toLazyText . pdBlocksToText
