module Korrvigs.Metadata.Media.OpenLibrary
  ( OpenLibraryQuery (..),
    queryOpenLibrary,
    parseQuery,
    parsePublishYear,
    parsePublishMonth,
  )
where

import Citeproc.Types (readAsInt)
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Data.Foldable (toList)
import Data.ISBN
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.Download
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Korrvigs.Utils
import Network.URI
import System.FilePath

data OpenLibraryQuery
  = OLISBN ISBN
  | OLKey Text
  | OLUrl Text
  deriving (Eq, Show)

data OLResult = OLResult
  { _olUrl :: Text,
    _olTitle :: Text,
    _olSubtitle :: Maybe Text,
    _olISBN10 :: [Text],
    _olISBN13 :: [Text],
    _olPublisher :: [Text],
    _olAuthors :: [Text],
    _olPublishDate :: Text,
    _olDescription :: Maybe Text,
    _olSeries :: [Text],
    _olCovers :: [Int]
  }
  deriving (Ord, Eq, Show)

newtype OLAuthor = OLAuthor
  { _authName :: Text
  }
  deriving (Ord, Eq, Show)

makeLenses ''OLResult
makeLenses ''OLAuthor

parseQuery :: Text -> Maybe OpenLibraryQuery
parseQuery url | T.isPrefixOf openUrl url = Just $ OLUrl url
parseQuery txt = case validateISBN txt of
  Left _ -> Nothing
  Right isbn -> Just $ OLISBN isbn

parseAuthors :: Value -> Parser [Text]
parseAuthors =
  withArray "OLResult authors" $
    fmap toList . mapM (withObject "OLResult author" $ \obj -> obj .: "key")

parseDescription :: Maybe Value -> Parser (Maybe Text)
parseDescription Nothing = pure Nothing
parseDescription (Just (String txt)) = pure $ Just txt
parseDescription (Just (Object o)) = Just <$> o .: "value"
parseDescription (Just x) = unexpected x

instance FromJSON OLResult where
  parseJSON = withObject "OLResult" $ \obj ->
    OLResult . (openUrl <>)
      <$> obj .: "key"
      <*> obj .: "title"
      <*> obj .:? "subtitle"
      <*> ((obj .:? "isbn_10") <&> fromMaybe [])
      <*> ((obj .:? "isbn_13") <&> fromMaybe [])
      <*> obj .: "publishers"
      <*> ((obj .: "authors") >>= parseAuthors)
      <*> obj .: "publish_date"
      <*> ((obj .:? "description") >>= parseDescription)
      <*> ((obj .:? "series") <&> fromMaybe [])
      <*> ((obj .:? "covers") <&> fromMaybe [])

instance FromJSON OLAuthor where
  parseJSON = withObject "OL Author" $ \obj -> OLAuthor <$> obj .: "name"

openUrl :: Text
openUrl = "https://openlibrary.org"

mkAPIUrl :: OpenLibraryQuery -> Maybe Text
mkAPIUrl (OLISBN isbn) = Just $ openUrl <> "/isbn/" <> renderISBN isbn <> ".json"
mkAPIUrl (OLKey key) = Just $ openUrl <> "/books/" <> key <> ".json"
mkAPIUrl (OLUrl url) =
  case parseURI (T.unpack url) >>= (^? ix 2) . splitPath . uriPath of
    Just keyWithSlash -> mkAPIUrl $ OLKey $ T.pack $ init keyWithSlash
    Nothing -> Nothing

parsePublishMonth :: Text -> Maybe MonthOfYear
parsePublishMonth t
  | T.isPrefixOf "January" t || T.isPrefixOf "Jan" t = Just January
  | T.isPrefixOf "February" t || T.isPrefixOf "Feb" t = Just February
  | T.isPrefixOf "March" t || T.isPrefixOf "Mar" t = Just March
  | T.isPrefixOf "April" t || T.isPrefixOf "Apr" t = Just April
  | T.isPrefixOf "May" t || T.isPrefixOf "May" t = Just May
  | T.isPrefixOf "June" t || T.isPrefixOf "Jun" t = Just June
  | T.isPrefixOf "July" t || T.isPrefixOf "Jul" t = Just July
  | T.isPrefixOf "August" t || T.isPrefixOf "Aug" t = Just August
  | T.isPrefixOf "September" t || T.isPrefixOf "Sep" t = Just September
  | T.isPrefixOf "October" t || T.isPrefixOf "Oct" t = Just October
  | T.isPrefixOf "November" t || T.isPrefixOf "Nov" t = Just November
  | T.isPrefixOf "December" t || T.isPrefixOf "Dec" t = Just December
  | otherwise = Nothing

parsePublishYear :: Text -> Maybe Year
parsePublishYear t = toInteger <$> readAsInt yr
  where
    yr = T.takeEnd 4 t

queryAuthor :: (MonadKorrvigs m) => Text -> m (Maybe Text)
queryAuthor key = do
  let url = openUrl <> key <> ".json"
  content <- simpleHttpM url
  case eitherDecode <$> content of
    Just (Right author) -> pure $ Just $ author ^. authName
    _ -> pure Nothing

queryOpenLibrary :: (MonadKorrvigs m) => OpenLibraryQuery -> m (Maybe (Media, [Id]))
queryOpenLibrary q = case mkAPIUrl q of
  Nothing -> pure Nothing
  Just url -> do
    content <- simpleHttpM url
    case eitherDecode <$> content of
      Nothing -> pure Nothing
      Just (Left _) -> pure Nothing
      Just (Right olr) -> do
        let title = olr ^. olTitle
        dlCover <- fmap join $ forM (listToMaybe $ olr ^. olCovers) $ \cov -> do
          let coverUrl = "https://covers.openlibrary.org/b/id/" <> T.pack (show cov) <> "-L.jpg"
          let coverNew = NewDownloadedFile coverUrl $ def & neTitle ?~ title <> " cover"
          newFromUrl coverNew
        let fullTitle = title <> maybe "" (" - " <>) (olr ^. olSubtitle)
        authors <- fmap catMaybes $ mapM queryAuthor $ olr ^. olAuthors
        pure $
          Just
            ( Media
                { _medType = Book,
                  _medAbstract = T.replace "\r\n" "\n" <$> olr ^. olDescription,
                  _medBibtex = Nothing,
                  _medDOI = [],
                  _medISBN = mapMaybe parseISBN $ olr ^. olISBN10 <> olr ^. olISBN13,
                  _medISSN = [],
                  _medTitle = Just fullTitle,
                  _medAuthors = authors,
                  _medMonth = parsePublishMonth $ olr ^. olPublishDate,
                  _medYear = parsePublishYear $ olr ^. olPublishDate,
                  _medUrl = Just $ olr ^. olUrl,
                  _medRSS = Nothing,
                  _medSource = [],
                  _medPublisher = olr ^. olPublisher,
                  _medContainer =
                    listToMaybe $
                      MediaContainer
                        <$> olr ^. olSeries
                        <*> pure Nothing
                        <*> pure Nothing
                        <*> pure Nothing
                        <*> pure Nothing,
                  _medInstitution = [],
                  _medLicense = [],
                  _medCover = dlCover,
                  _medDiscussion = []
                },
              toList dlCover
            )
  where
    parseISBN :: Text -> Maybe ISBN
    parseISBN isbnT = case validateISBN isbnT of
      Right isbn -> Just isbn
      Left _ -> Nothing
