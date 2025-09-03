module Korrvigs.Metadata.Media.OMDB where

import Control.Lens hiding (noneOf)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.CaseInsensitive as CI
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Korrvigs.Utils
import Network.URI
import Text.Parsec
import Text.Parsec.Number

data OMDBType
  = OMDBMovie
  | OMDBSerie
  | OMDBEpisode

data OMDBData = OMDBData
  { _omdbTitle :: Text,
    _omdbReleased :: Maybe Day,
    _omdbDirector :: Maybe Text,
    _omdbWriter :: Maybe Text,
    _omdbActor :: [Text],
    _omdbPlot :: Text,
    _omdbLanguage :: [Text],
    _omdbPoster :: Maybe Text,
    _omdbType :: OMDBType,
    _omdbSeries :: Maybe Text,
    _omdbLength :: Maybe Int -- In minutes
  }

makeLenses ''OMDBData

listOf :: Maybe Text -> [Text]
listOf Nothing = []
listOf (Just "N/A") = []
listOf (Just txt) = T.strip <$> T.split (== ',') txt

jsDay :: Text -> Parser Day
jsDay = parseTimeM True defaultTimeLocale "%e %b %Y" . T.unpack

liftMaybe :: (a -> Parser b) -> Maybe a -> Parser (Maybe b)
liftMaybe _ Nothing = pure Nothing
liftMaybe f (Just x) = Just <$> f x

naTxt :: Maybe Text -> Maybe Text
naTxt Nothing = Nothing
naTxt (Just "N/A") = Nothing
naTxt (Just v) = Just v

parseType :: Text -> Parser OMDBType
parseType "movie" = pure OMDBMovie
parseType "series" = pure OMDBSerie
parseType "episode" = pure OMDBEpisode
parseType t = fail $ "Unknown omdb type: " <> T.unpack t

parseMinutes :: Maybe Text -> Maybe Int
parseMinutes Nothing = Nothing
parseMinutes (Just txt) = case runParser minP () "" txt of
  Left _ -> Nothing
  Right n -> Just n
  where
    minP = do
      n <- decimal
      void $ string " min"
      eof
      pure n

instance FromJSON OMDBData where
  parseJSON = withObject "OMDB" $ \obj ->
    OMDBData
      <$> obj .: "Title"
      <*> (liftMaybe jsDay =<< obj .:? "Released")
      <*> (naTxt <$> obj .:? "Director")
      <*> (naTxt <$> obj .:? "Writer")
      <*> (listOf <$> obj .:? "Actors")
      <*> obj .: "Plot"
      <*> (listOf <$> obj .:? "Language")
      <*> obj .:? "Poster"
      <*> (parseType =<< obj .: "Type")
      <*> obj .:? "seriesID"
      <*> (parseMinutes <$> obj .:? "Runtime")

type OMDBId = Text

imdbUrl :: Text
imdbUrl = "https://www.imdb.com/"

parseQuery :: Text -> Maybe OMDBId
parseQuery url | T.isPrefixOf imdbUrl url = do
  uri <- parseURI $ T.unpack url
  case runParser urlP () "" (uriPath uri) of
    Left _ -> Nothing
    Right i -> pure i
  where
    urlP = do
      void $ string "/title/"
      i <- manyTill (noneOf "/") $ optional (char '/') >> eof
      pure $ T.pack i
parseQuery _ = Nothing

queryOMDB :: (MonadKorrvigs m) => OMDBId -> m (Maybe (NewEntry -> NewEntry))
queryOMDB i =
  getCredential "omdb" >>= \case
    Nothing -> pure Nothing
    Just key -> queryOMDBWithKey key i

queryOMDBWithKey :: (MonadKorrvigs m) => Text -> OMDBId -> m (Maybe (NewEntry -> NewEntry))
queryOMDBWithKey key i = do
  let url = "https://www.omdbapi.com/?apikey=" <> key <> "&i=" <> i <> "&plot=full"
  content <- simpleHttpM url
  case eitherDecode <$> content of
    Just (Right omdb) -> do
      let imUrl = imdbUrl <> "title/" <> i
      let authors = nubOrd $ mconcat [toList (omdb ^. omdbDirector), toList (omdb ^. omdbWriter), omdb ^. omdbActor]
      let isFr = any (\l -> CI.mk l == "fr" || CI.mk l == "french") $ omdb ^. omdbLanguage
      let tp = case omdb ^. omdbType of
            OMDBMovie -> Movie
            OMDBSerie -> Show
            OMDBEpisode -> Episode
      pure $
        Just $
          foldr
            (.)
            (setMtdtValue MediaMtdt tp)
            [ setMtdtValue Abstract $ omdb ^. omdbPlot,
              neTitle ?~ omdb ^. omdbTitle,
              setMtdtValue Authors authors,
              maybe id (neDate ?~) $ omdb ^. omdbReleased,
              setMtdtValue Url imUrl,
              if isFr then setMtdtValue Language "fr" else id,
              neCover .~ omdb ^. omdbPoster
            ]
    _ -> pure Nothing
