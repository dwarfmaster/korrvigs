module Korrvigs.Link.Download.Books where

import Control.Lens
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Text.HTML.TagSoup

manytoon :: (MonadKorrvigs m) => Text -> [Tag Text] -> m (Endo NewEntry)
manytoon url _
  | "https://manytoon.com/comic/" `T.isPrefixOf` url =
      pure $ Endo $ setMtdtValue MediaMtdt Manga . delFeed . fixTitle
  where
    delFeed = neMtdt . at (mtdtName Feed) .~ Nothing
    fixTitle = neTitle . _Just %~ cutTitle
    cutTitle = T.intercalate " - " . init . T.splitOn " - "
manytoon _ _ = pure mempty

goodreads :: (MonadKorrvigs m) => Text -> [Tag Text] -> m (Endo NewEntry)
goodreads url _
  | "https://www.goodreads.com/book" `T.isPrefixOf` url =
      pure $ Endo $ setMtdtValue MediaMtdt Book . fixTitle
  where
    fixTitle = neTitle . _Just %~ cutTitle
    cutTitle = maybe "" fst . uncons . T.splitOn "|"
goodreads _ _ = pure mempty

bedetheque :: (MonadKorrvigs m) => Text -> [Tag Text] -> m (Endo NewEntry)
bedetheque url _
  | "https://www.bedetheque.com/" `T.isPrefixOf` url =
      pure $ Endo $ setMtdtValue MediaMtdt Comic . setMtdtValue Language "fr" . delFeed
  where
    delFeed = neMtdt . at (mtdtName Feed) .~ Nothing
bedetheque _ _ = pure mempty

webtoons :: (MonadKorrvigs m) => Text -> [Tag Text] -> m (Endo NewEntry)
webtoons url tags
  | "https://www.webtoons.com/" `T.isPrefixOf` url =
      pure $ foldMap matchFeed tags <> Endo (setMtdtValue MediaMtdt Webcomic)
  where
    matchFeed :: Tag Text -> Endo NewEntry
    matchFeed tag@(TagOpen "a" attrs)
      | tag ~== TagOpen ("a" :: Text) [("title", "Rss")] =
          case lookup "href" attrs of
            Nothing -> mempty
            Just feed -> Endo $ setMtdtValue Feed feed
    matchFeed _ = mempty
webtoons _ _ = pure mempty
