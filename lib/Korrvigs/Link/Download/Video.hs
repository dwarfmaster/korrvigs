module Korrvigs.Link.Download.Video (nebula) where

import Control.Lens
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Network.URI
import Opaleye
import System.FilePath
import Text.HTML.TagSoup

setVideo :: Endo NewEntry
setVideo = Endo $ setMtdtValue MediaMtdt Video

setParent :: (MonadKorrvigs m) => First Text -> m (Endo NewEntry)
setParent author =
  case author of
    First Nothing -> pure setVideo
    First (Just auth) -> do
      parent <- rSelectOne $ do
        mtdt <- selectTable entriesMetadataTable
        where_ $ mtdt ^. sqlKey .== sqlStrictText (mtdtSqlName Url)
        where_ $ mtdt ^. sqlValue .== sqlValueJSONB auth
        pure $ mtdt ^. sqlEntry
      let ex = case parent of
            Nothing -> mempty
            Just parentId -> Endo $ neParents %~ (parentId :)
      pure $ ex <> setVideo

nebula :: (MonadKorrvigs m) => Text -> [Tag Text] -> m (Endo NewEntry)
nebula url tags
  | "https://nebula.tv/videos/" `T.isPrefixOf` url =
      setParent $ foldMap matchAuthor tags
  where
    matchAuthor :: Tag Text -> First Text
    matchAuthor tagOpen@(TagOpen "link" attrs)
      | tagOpen ~== TagOpen ("link" :: Text) [("rel", "alternate"), ("type", "application/rss+xml")] = case lookup "href" attrs >>= parseURI . T.unpack of
          Nothing -> mempty
          Just rss ->
            let channel = takeBaseName $ uriPath rss
             in pure $ "https://nebula.tv/" <> T.pack channel
    matchAuthor _ = mempty
nebula url _
  | "https://nebula.tv/" `T.isPrefixOf` url =
      pure $ Endo $ setMtdtValue MediaMtdt Channel
nebula _ _ = pure mempty
