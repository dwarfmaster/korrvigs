module Korrvigs.Metadata.Media.MangaUpdates
  ( isMangaUpdates,
    queryMangaUpdates,
  )
where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Monoid
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Metadata.Media.OpenLibrary (parsePublishMonth, parsePublishYear)
import Korrvigs.Monad
import Korrvigs.Utils
import Text.HTML.TagSoup

data MangaUpdatesData = MangaUpdatesData
  { _muName :: Text,
    _muUrl :: Text,
    _muDescription :: Text,
    _muAuthors :: [Text],
    _muDate :: Text,
    _muPublishers :: [Text],
    _muImage :: Text
  }

makeLenses ''MangaUpdatesData

pL :: (Value -> Parser a) -> Array -> Parser [a]
pL parser = fmap toList . mapM parser

parseName :: Value -> Parser Text
parseName = withObject "MangaUpdates named" (.: "name")

instance FromJSON MangaUpdatesData where
  parseJSON = withObject "MangaUpdates data" $ \obj ->
    MangaUpdatesData
      <$> obj .: "name"
      <*> obj .: "url"
      <*> obj .: "description"
      <*> (obj .: "author" >>= withArray "MangaUpdates author" (pL parseName))
      <*> obj .: "datePublished"
      <*> (obj .: "publisher" >>= withArray "MangaUpdates publisher" (pL parseName))
      <*> obj .: "image"

isMangaUpdates :: Text -> Maybe Text
isMangaUpdates url =
  if T.isPrefixOf "https://www.mangaupdates.com" url
    then Just url
    else Nothing

queryMangaUpdates :: (MonadKorrvigs m) => Text -> m (Maybe (NewEntry -> NewEntry))
queryMangaUpdates url = do
  content <- simpleHttpM url
  let tags = parseTags <$> content
  ldDat <- case sections isLD <$> tags of
    Just ((_ : TagText js : _) : _) -> case decode js of
      Nothing -> pure Nothing
      Just muData -> do
        let title = muData ^. muName
        pure $
          Just $
            foldr
              (.)
              (setMtdtValue MediaMtdt Manga)
              [ setMtdtValue Abstract $ muData ^. muDescription,
                neTitle ?~ title,
                setMtdtValue Authors $ muData ^. muAuthors,
                setMtdtValueM MedMonth $ parsePublishMonth $ muData ^. muDate,
                setMtdtValueM MedYear $ parsePublishYear $ muData ^. muDate,
                setMtdtValue Url $ muData ^. muUrl,
                setMtdtValue Publisher $ muData ^. muPublishers,
                neCover ?~ muData ^. muImage
              ]
    _ -> pure Nothing
  let forum = flip fmap tags $ \tgs -> flip foldMap tgs $ \case
        tag@(TagOpen "a" attrs)
          | tag ~== TagOpen ("a" :: ByteString) [("title", "Click for Forum Info")] ->
              maybe mempty (\lnk -> Endo $ setMtdtValue Discussions [LT.toStrict $ LEnc.decodeUtf8 lnk]) $ lookup "href" attrs
        _ -> mempty
  pure $ (maybe id appEndo forum .) <$> ldDat
  where
    isLD :: (Eq str, IsString str) => Tag str -> Bool
    isLD (TagOpen "script" attrs) = ("type", "application/ld+json") `elem` attrs
    isLD _ = False
