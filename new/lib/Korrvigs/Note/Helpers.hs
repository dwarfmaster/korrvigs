module Korrvigs.Note.Helpers where

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
inlineToText (Sidenote _) = ""

inlinesToText :: [Inline] -> Builder
inlinesToText = mconcat . fmap inlineToText

renderInlines :: [Inline] -> Text
renderInlines = toStrict . toLazyText . inlinesToText
