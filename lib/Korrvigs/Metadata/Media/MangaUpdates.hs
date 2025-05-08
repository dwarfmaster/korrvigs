module Korrvigs.Metadata.Media.MangaUpdates
  ( isMangaUpdates,
    queryMangaUpdates,
  )
where

import Conduit
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Data.Foldable
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.Download
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Metadata.Media.OpenLibrary (parsePublishMonth, parsePublishYear)
import Korrvigs.Monad
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
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

queryMangaUpdates :: (MonadKorrvigs m) => Text -> m (Maybe (Media, [Id]))
queryMangaUpdates url = do
  req <- parseRequest $ T.unpack url
  man <- liftIO $ newManager tlsManagerSettings
  content <- runResourceT $ do
    resp <- http req man
    let scode = statusCode (responseStatus resp)
    if scode == 200
      then fmap Just $ runConduit $ responseBody resp .| sinkLazy
      else pure Nothing
  case sections isLD . parseTags <$> content of
    Just ((_ : TagText js : _) : _) -> case decode js of
      Nothing -> pure Nothing
      Just muData -> do
        let title = muData ^. muName
        dlCover <- do
          let coverNew = NewDownloadedFile (muData ^. muImage) $ def & neTitle ?~ title <> " cover"
          newFromUrl coverNew
        pure $
          Just
            ( Media
                { _medType = Manga,
                  _medAbstract = Just $ muData ^. muDescription,
                  _medBibtex = Nothing,
                  _medDOI = [],
                  _medISBN = [],
                  _medISSN = [],
                  _medTitle = Just title,
                  _medAuthors = muData ^. muAuthors,
                  _medMonth = parsePublishMonth $ muData ^. muDate,
                  _medYear = parsePublishYear $ muData ^. muDate,
                  _medUrl = Just $ muData ^. muUrl,
                  _medRSS = Nothing,
                  _medSource = [],
                  _medPublisher = muData ^. muPublishers,
                  _medContainer = Nothing,
                  _medInstitution = [],
                  _medLicense = [],
                  _medCover = dlCover
                },
              toList dlCover
            )
    _ -> pure Nothing
  where
    isLD :: (Eq str, IsString str) => Tag str -> Bool
    isLD (TagOpen "script" attrs) = ("type", "application/ld+json") `elem` attrs
    isLD _ = False
