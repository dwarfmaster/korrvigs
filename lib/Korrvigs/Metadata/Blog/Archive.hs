module Korrvigs.Metadata.Blog.Archive where

import Control.Lens
import Data.Foldable (fold)
import qualified Data.Foldable1 as F1
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Time
import Korrvigs.Metadata.Blog.Export (BlogPageContent (..), renderPageContent, renderRssIcon)
import Korrvigs.Metadata.Blog.Structure
import Korrvigs.Monad
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Yesod

generateArchivePage :: (MonadKorrvigs m) => BlogConfig -> (BlogUrl -> m Text) -> (Maybe Text) -> Html -> [(Text, Day, Text)] -> m Html
generateArchivePage cfg renderUrl tag extra entries = do
  mtdt <- loadMtdt cfg
  preppedEntries <- mapM (\(u, d, t) -> (,d,t) <$> renderUrl (BlogPostNote u)) entries
  let byYear = NE.groupBy (\(_, d1, _) (_, d2, _) -> getYear d1 == getYear d2) preppedEntries
  icon <- renderRssIcon renderUrl tag
  let content =
        mconcat
          [ H.h1 $ icon <> " " <> H.toMarkup title,
            mconcat $ renderYear <$> byYear,
            extra
          ]
  renderPageContent $ BlogPageContent content mtdt title renderUrl
  where
    title = maybe "Archive" ("Archive for " <>) tag
    getYear :: Day -> Year
    getYear = dayPeriod
    renderYear :: NE.NonEmpty (Text, Day, Text) -> Html
    renderYear yearEntries =
      mconcat
        [ H.h2 $ H.toMarkup $ show $ getYear $ view _2 $ F1.head $ yearEntries,
          H.ul $ fold $ H.li . renderEntry <$> yearEntries
        ]
    renderEntry (u, d, t) =
      mconcat
        [ H.span (H.toMarkup $ formatTime defaultTimeLocale "%F" d) ! A.class_ "postdate",
          " ",
          H.a (H.toMarkup t) ! A.href (H.toValue u)
        ]
