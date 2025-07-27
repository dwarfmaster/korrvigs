module Korrvigs.Metadata.Media.MusicBrainz where

import Conduit
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time.Calendar
import Data.Time.Format
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.Download
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.URI
import System.FilePath

data MBReleaseData = MBReleaseData
  { _mbrId :: Text,
    _mbrTitle :: Text,
    _mbrCountry :: Maybe Text,
    _mbrArtists :: [Text],
    _mbrCover :: Bool,
    _mbrDate :: Maybe Day
  }

data MBRecordingData = MBRecordingData
  { _mbroId :: Text,
    _mbroTitle :: Text,
    _mbroArtists :: [Text],
    _mbroLength :: Maybe Double, -- In minutes
    _mbroDate :: Maybe Day,
    _mbroReleases :: [Text] -- Ids of the releases
  }

makeLenses ''MBReleaseData
makeLenses ''MBRecordingData

parseArtists :: Value -> Parser [Text]
parseArtists = withArray "MusicBrainz Artists" $ \arr ->
  fmap toList $ forM arr $ withObject "MusicBrainz Artist" (.: "name")

jsDay :: Text -> Parser Day
jsDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

liftMaybe :: (a -> Parser b) -> Maybe a -> Parser (Maybe b)
liftMaybe _ Nothing = pure Nothing
liftMaybe f (Just x) = Just <$> f x

instance FromJSON MBReleaseData where
  parseJSON = withObject "MusicBrainz Release" $ \obj ->
    MBReleaseData
      <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .:? "country"
      <*> (parseArtists =<< obj .: "artist-credit")
      <*> (withObject "MusicBrainz Cover" (.: "front") =<< obj .: "cover-art-archive")
      <*> (liftMaybe jsDay =<< obj .:? "date")

instance FromJSON MBRecordingData where
  parseJSON = withObject "MusicBrainz Recording" $ \obj ->
    MBRecordingData
      <$> obj .: "id"
      <*> obj .: "title"
      <*> (parseArtists =<< obj .: "artist-credit")
      <*> (fmap toMin <$> obj .:? "length")
      <*> (liftMaybe jsDay =<< obj .:? "date")
      <*> (parseReleases =<< obj .: "releases")
    where
      toMin = (/ 6000.0)
      parseReleases = withArray "MusicBrainz Releases" $ \arr ->
        fmap toList $ forM arr $ withObject "MusicBrainz recording release" (.: "id")

data MBId
  = MBRelease Text
  | MBRecording Text

mbUrl :: Text
mbUrl = "https://musicbrainz.org/"

parseQuery :: Text -> Maybe MBId
parseQuery url | T.isPrefixOf mbUrl url = do
  uri <- parseURI $ T.unpack url
  case splitFileName $ uriPath uri of
    ("/release/", i) -> pure $ MBRelease $ T.pack i
    ("/recording/", i) -> pure $ MBRecording $ T.pack i
    _ -> Nothing
parseQuery _ = Nothing

queryMB :: (MonadKorrvigs m) => MBId -> m (Maybe (Media, [Id]))
queryMB (MBRelease i) = doQuery (mbUrl <> "ws/2/release/" <> i <> "?fmt=json&inc=artists") $ \mbr -> do
  covId <- forM (guard (mbr ^. mbrCover) :: Maybe ()) $ \() -> do
    coverArtReq' <- parseRequest $ "https://coverartarchive.org/release/" <> T.unpack i <> "/front"
    let coverArtReq = coverArtReq' {redirectCount = 0}
    man <- liftIO $ newManager tlsManagerSettings
    coverUrl <- runResourceT $ do
      resp <- http coverArtReq man
      let scode = statusCode $ responseStatus resp
      if scode == 307
        then pure $ lookup "Location" $ responseHeaders resp
        else pure Nothing
    fmap join $ forM coverUrl $ \covUrl -> do
      let covNew = NewDownloadedFile (Enc.decodeUtf8 covUrl) $ def & neTitle ?~ (mbr ^. mbrTitle) <> " cover"
      newFromUrl covNew
  pure
    ( Media
        { _medType = Album,
          _medAbstract = Nothing,
          _medBibtex = Nothing,
          _medDOI = [],
          _medISBN = [],
          _medISSN = [],
          _medTitle = Just $ mbr ^. mbrTitle,
          _medAuthors = mbr ^. mbrArtists,
          _medMonth = mbr ^? mbrDate . _Just . to toGregorian . _2,
          _medYear = mbr ^? mbrDate . _Just . to toGregorian . _1,
          _medUrl = Just $ mbUrl <> "release/" <> i,
          _medRSS = Nothing,
          _medSource = [],
          _medPublisher = [],
          _medContainer = Nothing,
          _medInstitution = [],
          _medLicense = [],
          _medCover = join covId,
          _medDiscussion = []
        },
      toList $ join covId
    )
queryMB (MBRecording i) = doQuery (mbUrl <> "ws/2/recording/" <> i <> "?fmt=json&inc=artists+releases") $ \mbr ->
  pure
    ( Media
        { _medType = Song,
          _medAbstract = Nothing,
          _medBibtex = Nothing,
          _medDOI = [],
          _medISBN = [],
          _medISSN = [],
          _medTitle = Just $ mbr ^. mbroTitle,
          _medAuthors = mbr ^. mbroArtists,
          _medMonth = mbr ^? mbroDate . _Just . to toGregorian . _2,
          _medYear = mbr ^? mbroDate . _Just . to toGregorian . _1,
          _medUrl = Just $ mbUrl <> "recording/" <> i,
          _medRSS = Nothing,
          _medSource = [],
          _medPublisher = [],
          _medContainer = Nothing,
          _medInstitution = [],
          _medLicense = [],
          _medCover = Nothing,
          _medDiscussion = []
        },
      []
    )

doQuery :: (MonadKorrvigs m, FromJSON a) => Text -> (a -> m (Media, [Id])) -> m (Maybe (Media, [Id]))
doQuery url mkMedia = do
  req' <- parseRequest $ T.unpack url
  let req = req' {requestHeaders = ("User-Agent", mbUserAgent) : requestHeaders req'}
  man <- liftIO $ newManager tlsManagerSettings
  content <- runResourceT $ do
    resp <- http req man
    let scode = statusCode $ responseStatus resp
    if scode == 200
      then fmap Just $ runConduit $ responseBody resp .| sinkLazy
      else pure Nothing
  case eitherDecode <$> content of
    Just (Right v) -> Just <$> mkMedia v
    _ -> pure Nothing
  where
    mbUserAgent = "korrvigs/1.0 (korrvigs@dwarfmaster.net)"
