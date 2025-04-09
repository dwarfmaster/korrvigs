module Korrvigs.Metadata.Media.OpenLibrary
  ( OpenLibraryQuery (..),
    queryOpenLibrary,
    parseQuery,
    parsePublishYear,
    parsePublishMonth,
  )
where

import Citeproc.Types (readAsInt)
import Conduit (runResourceT)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Conduit
import Data.Conduit.Combinators (sinkLazy)
import Data.Default
import Data.Foldable (toList)
import Data.ISBN
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.Download
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
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

makeLenses ''OLResult

parseQuery :: Text -> Maybe OpenLibraryQuery
parseQuery url | T.isPrefixOf openUrl url = Just $ OLUrl url
parseQuery txt = case validateISBN txt of
  Left _ -> Nothing
  Right isbn -> Just $ OLISBN isbn

parseAuthors :: Value -> Parser [Text]
parseAuthors =
  withArray "OLResult authors" $
    fmap toList . mapM (withObject "OLResult author" $ \obj -> obj .: "name")

parseDescription :: Maybe Value -> Parser (Maybe Text)
parseDescription Nothing = pure Nothing
parseDescription (Just (String txt)) = pure $ Just txt
parseDescription (Just (Object o)) = Just <$> o .: "value"
parseDescription (Just x) = unexpected x

instance FromJSON OLResult where
  parseJSON = withObject "OLResult" $ \obj ->
    OLResult
      <$> obj .: "info_url"
      <*> ((obj .: "details") >>= (.: "title"))
      <*> ((obj .: "details") >>= (.:? "subtitle"))
      <*> ((obj .: "details") >>= (.: "isbn_10"))
      <*> ((obj .: "details") >>= (.: "isbn_13"))
      <*> ((obj .: "details") >>= (.: "publishers"))
      <*> ((obj .: "details") >>= (.: "authors") >>= parseAuthors)
      <*> ((obj .: "details") >>= (.: "publish_date"))
      <*> ((obj .: "details") >>= (.:? "description") >>= parseDescription)
      <*> ((obj .: "details") >>= (.:? "series") <&> fromMaybe [])
      <*> ((obj .: "details") >>= (.:? "covers") <&> fromMaybe [])

openUrl :: Text
openUrl = "https://openlibrary.org"

mkAPIBaseUrl :: OpenLibraryQuery -> Maybe Text
mkAPIBaseUrl (OLISBN isbn) = Just $ openUrl <> "/api/books?bibkeys=ISBN:" <> renderISBN isbn
mkAPIBaseUrl (OLKey key) = Just $ openUrl <> "/api/books?bibkeys=OLID:" <> key
mkAPIBaseUrl (OLUrl url) =
  case parseURI (T.unpack url) >>= (^? ix 2) . splitPath . uriPath of
    Just keyWithSlash -> mkAPIBaseUrl $ OLKey $ T.pack $ init keyWithSlash
    Nothing -> Nothing

mkAPIUrl :: OpenLibraryQuery -> Maybe Text
mkAPIUrl = fmap (<> "&format=json&jscmd=details") . mkAPIBaseUrl

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

queryOpenLibrary :: (MonadKorrvigs m) => OpenLibraryQuery -> m (Maybe (Media, [Id]))
queryOpenLibrary q = case mkAPIUrl q of
  Nothing -> pure Nothing
  Just url -> do
    req <- parseRequest $ T.unpack url
    man <- liftIO $ newManager tlsManagerSettings
    content <- runResourceT $ do
      resp <- http req man
      let scode = statusCode (responseStatus resp)
      if scode == 200
        then fmap Just $ runConduit $ responseBody resp .| sinkLazy
        else pure Nothing
    case extract . eitherDecode <$> content of
      Nothing -> pure Nothing
      Just [] -> pure Nothing
      Just (olr : _) -> do
        let title = olr ^. olTitle
        dlCover <- fmap join $ forM (listToMaybe $ olr ^. olCovers) $ \cov -> do
          let coverUrl = "https://covers.openlibrary.org/b/id/" <> T.pack (show cov) <> "-L.jpg"
          let coverNew = NewDownloadedFile coverUrl $ def & neTitle ?~ title <> " cover"
          newFromUrl coverNew
        let fullTitle = title <> maybe "" (" - " <>) (olr ^. olSubtitle)
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
                  _medAuthors = olr ^. olAuthors,
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
                  _medCover = dlCover
                },
              toList dlCover
            )
  where
    parseISBN :: Text -> Maybe ISBN
    parseISBN isbnT = case validateISBN isbnT of
      Right isbn -> Just isbn
      Left _ -> Nothing
    extract :: Either a (Map Text c) -> [c]
    extract (Left _) = []
    extract (Right mp) = M.elems mp
