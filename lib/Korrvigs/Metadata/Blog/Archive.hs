module Korrvigs.Metadata.Blog.Archive where

import Control.Lens
import Data.Foldable (fold)
import qualified Data.Foldable1 as F1
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Text (Text)
import Data.Time
import Korrvigs.Metadata.Blog.Export (BlogPageContent (..), renderPageContent, renderRssIcon)
import Korrvigs.Metadata.Blog.Structure
import Korrvigs.Monad
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Yesod

generateArchivePage :: (MonadKorrvigs m) => Map Text Text -> Bool -> BlogMenuContent -> (BlogUrl -> m Text) -> (Maybe Text) -> [Text] -> m Html
generateArchivePage mtdt onlyPublished menuContent renderUrl tag tags = do
  entries <- loadForTag onlyPublished tag Nothing
  extra <- alltags <$> mapM prepTag tags
  preppedEntries <- mapM (\(u, d, t, _) -> (,d,t) <$> renderUrl (BlogPostNote u)) entries
  let byYear = NE.groupBy (\(_, d1, _) (_, d2, _) -> getYear d1 == getYear d2) preppedEntries
  icon <- renderRssIcon renderUrl tag
  let content =
        mconcat
          [ H.h1 $ icon <> " " <> H.toMarkup title,
            mconcat $ renderYear <$> byYear,
            extra
          ]
  let url = maybe BlogArchive BlogArchiveTag tag
  renderPageContent $ BlogPageContent content mtdt title renderUrl menuContent url
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
    alltags [] = mempty
    alltags preppedTags =
      mconcat
        [ H.h2 "Tags",
          H.ul $ mconcat preppedTags
        ]
    prepTag t = do
      u <- renderUrl $ BlogArchiveTag t
      pure $ H.li $ H.a (H.toMarkup t) ! A.href (H.toValue u)
