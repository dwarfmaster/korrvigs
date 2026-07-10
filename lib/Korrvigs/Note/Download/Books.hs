module Korrvigs.Note.Download.Books where

import Control.Lens
import Data.Aeson
import Data.List.Split (divvy)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Text.HTML.TagSoup
import Text.Read (readMaybe)

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
bedetheque url tags
  | any (`T.isPrefixOf` url) bdgestUrls =
      pure $ foldMap matchSpans (divvy 2 1 tags) <> foldMap matchMeta tags <> common
  where
    bdgestUrls = ["https://www.bedetheque.com/", "https://www.bdgest.com/"]
    common = Endo $ setMtdtValue MediaMtdt Comic . setMtdtValue Language "fr" . delFeed
    delFeed = neMtdt . at (mtdtName Feed) .~ Nothing
    matchMeta :: Tag Text -> Endo NewEntry
    matchMeta tag@(TagOpen "meta" attrs)
      | tag ~== TagOpen ("meta" :: Text) [("itemprop", "datePublished")] =
          case lookup "content" attrs >>= parseDate of
            Nothing -> mempty
            Just day -> Endo $ neDate %~ Just . fromMaybe day
    matchMeta tag@(TagOpen "meta" attrs)
      | tag ~== TagOpen ("meta" :: Text) [("itemprop", "description")] =
          case lookup "content" attrs of
            Nothing -> mempty
            Just desc -> Endo $ neMtdt . at (mtdtName Abstract) ?~ toJSON desc
    matchMeta _ = mempty
    parseDate :: Text -> Maybe Day
    parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack
    matchSpans :: [Tag Text] -> Endo NewEntry
    matchSpans [tag@(TagOpen "span" _), TagText isbn]
      | tag ~== TagOpen ("span" :: Text) [("itemprop", "isbn")] =
          Endo $ neMtdt . at (mtdtName ISBNMtdt) %~ Just . fromMaybe (toJSON [isbn])
    matchSpans [tag@(TagOpen "span" _), TagText pages]
      | tag ~== TagOpen ("span" :: Text) [("itemprop", "numberOfPages")] =
          case readMaybe (T.unpack pages) of
            Nothing -> mempty
            Just (pgs :: Int) ->
              Endo $ neMtdt . at (mtdtName Pages) %~ Just . fromMaybe (toJSON pgs)
    matchSpans _ = mempty
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
