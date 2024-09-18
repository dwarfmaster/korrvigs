module Korrvigs.FTS.Render where

import Data.List
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Korrvigs.FTS.Query

renderQuery :: Query -> Text
renderQuery = toStrict . toLazyText . renderOr

renderOr :: Query -> Builder
renderOr (Or qs) = mconcat $ intersperse " or " $ renderAnd <$> qs
renderOr q = renderAnd q

renderAnd :: Query -> Builder
renderAnd (And qs) = mconcat $ intersperse " and " $ renderPhrase <$> qs
renderAnd q = renderPhrase q

renderPhrase :: Query -> Builder
renderPhrase (Not q) = "not " <> renderQuote q
renderPhrase q = renderQuote q

renderQuote :: Query -> Builder
renderQuote (Phrase txts) = "\"" <> mconcat (intersperse " " $ fromText <$> txts) <> "\""
renderQuote q = "(" <> renderOr q <> ")"
