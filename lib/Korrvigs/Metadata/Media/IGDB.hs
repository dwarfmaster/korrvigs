module Korrvigs.Metadata.Media.IGDB where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Default
import Data.Foldable
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.Download
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Korrvigs.Utils
import Network.HTTP.Conduit
import Network.HTTP.Types.Method

-- Access token handling
data IGDBCredentials = IGDBCredentials
  { _credId :: Text,
    _credSecret :: Text
  }

makeLenses ''IGDBCredentials

instance FromJSON IGDBCredentials where
  parseJSON = withObject "IGDB credentials" $ \obj ->
    IGDBCredentials
      <$> obj .: "id"
      <*> obj .: "secret"

newtype AccessToken = AK {unAK :: Text}

data AKResponse = AKResponse
  { _akToken :: AccessToken,
    _akExpires :: Integer,
    _akType :: Text
  }

makeLenses ''AKResponse

instance FromJSON AKResponse where
  parseJSON = withObject "IGDB Access Token" $ \obj ->
    AKResponse . AK
      <$> obj .: "access_token"
      <*> obj .: "expires_in"
      <*> obj .: "token_type"

data StoredToken = StoredToken
  { _stToken :: AccessToken,
    _stExpires :: LocalTime
  }

makeLenses ''StoredToken

instance FromJSON StoredToken where
  parseJSON = withObject "Stored IGDB Access Token" $ \obj ->
    StoredToken . AK
      <$> obj .: "token"
      <*> obj .: "expires"

instance ToJSON StoredToken where
  toJSON st =
    object
      [ "token" .= unAK (st ^. stToken),
        "expires" .= (st ^. stExpires)
      ]

newAccessToken :: (MonadKorrvigs m) => IGDBCredentials -> m AccessToken
newAccessToken cred = do
  let url = "https://id.twitch.tv/oauth2/token?client_id=" <> cred ^. credId <> "&client_secret=" <> cred ^. credSecret <> "&grant_type=client_credentials"
  req <- parseRequest $ T.unpack url
  resp <- reqHttpM $ req {method = methodPost}
  case eitherDecode <$> resp of
    Nothing -> throwM $ KMiscError "Failed to get access token for IGDB"
    Just (Left err) -> throwM $ KMiscError $ "Failed to decode acces token for IGDB: " <> T.pack err
    Just (Right tok) -> do
      tm <- zonedTimeToLocalTime <$> liftIO getZonedTime
      let expires = addLocalTime (fromInteger (tok ^. akExpires) - 10) tm
      let stored =
            StoredToken
              { _stToken = tok ^. akToken,
                _stExpires = expires
              }
      storeToken "igdb" stored
      pure $ tok ^. akToken

getAccessToken :: (MonadKorrvigs m) => IGDBCredentials -> m AccessToken
getAccessToken creds =
  getToken "igdb" >>= \case
    Just tok -> do
      tm <- zonedTimeToLocalTime <$> liftIO getZonedTime
      if tm > tok ^. stExpires
        then newAccessToken creds
        else pure $ tok ^. stToken
    Nothing -> newAccessToken creds

-- Game data
type IGDBId = Int

data IGDBData = IGDBData
  { _igdbCover :: Maybe IGDBId,
    _igdbRelease :: Maybe Integer,
    _igdbReleases :: [IGDBId],
    _igdbCompanies :: [IGDBId],
    _igdbName :: Text,
    _igdbSummary :: Text,
    _igdbSlug :: Text
  }

data IGDBInvolvedCompany = IGDBInvolvedCompany
  { _invId :: IGDBId,
    _invCompany :: IGDBId,
    _invDev :: Bool,
    _invPublisher :: Bool
  }

data IGDBCompany = IGDBCompany
  { _compId :: IGDBId,
    _compName :: Text
  }

data IGDBCover = IGDBCover
  { _coverId :: IGDBId,
    _coverUrl :: Text
  }

data IGDBReleaseDate = IGDBReleaseDate
  { _rdId :: IGDBId,
    _rdDate :: Integer -- UNIX timestamp
  }

makeLenses ''IGDBData
makeLenses ''IGDBInvolvedCompany
makeLenses ''IGDBCompany
makeLenses ''IGDBCover
makeLenses ''IGDBReleaseDate

instance FromJSON IGDBData where
  parseJSON = withObject "IGDB Game" $ \obj ->
    IGDBData
      <$> obj .:? "cover"
      <*> obj .:? "first_release_date"
      <*> (fromMaybe [] <$> obj .:? "release_dates")
      <*> (fromMaybe [] <$> obj .:? "involved_companies")
      <*> obj .: "name"
      <*> obj .: "summary"
      <*> obj .: "slug"

instance FromJSON IGDBInvolvedCompany where
  parseJSON = withObject "IGDB Involved Company" $ \obj ->
    IGDBInvolvedCompany
      <$> obj .: "id"
      <*> obj .: "company"
      <*> obj .: "developer"
      <*> obj .: "publisher"

instance FromJSON IGDBCompany where
  parseJSON = withObject "IGDB Company" $ \obj ->
    IGDBCompany
      <$> obj .: "id"
      <*> obj .: "name"

instance FromJSON IGDBCover where
  parseJSON = withObject "IGDB Cover" $ \obj ->
    IGDBCover
      <$> obj .: "id"
      <*> obj .: "url"

instance FromJSON IGDBReleaseDate where
  parseJSON = withObject "IGDB Release Date" $ \obj ->
    IGDBReleaseDate
      <$> obj .: "id"
      <*> obj .: "date"

doQueryIGDB :: (MonadKorrvigs m, FromJSON dat) => IGDBCredentials -> AccessToken -> Text -> Text -> [Text] -> m [dat]
doQueryIGDB creds token endpoint cond fields = do
  req' <- parseRequest $ T.unpack url
  let req =
        req'
          { method = methodPost,
            requestHeaders = [hclient, hauth, haccept] <> requestHeaders req',
            requestBody = RequestBodyBS body
          }
  resp <- reqHttpM req
  case eitherDecode <$> resp of
    Nothing -> throwM $ KMiscError $ "Failed to get " <> url
    Just (Left err) -> throwM $ KMiscError $ "Failed to decode response (" <> url <> "): " <> T.pack err
    Just (Right []) -> throwM $ KMiscError $ "No response for " <> url
    Just (Right vs) -> pure vs
  where
    url = "https://api.igdb.com/v4/" <> endpoint
    body = Enc.encodeUtf8 $ "fields " <> T.intercalate "," fields <> "; where " <> cond <> ";"
    haccept = ("Accept", "application/json")
    hauth = ("Authorization", Enc.encodeUtf8 $ "Bearer " <> unAK token)
    hclient = ("Client-ID", Enc.encodeUtf8 $ creds ^. credId)

parseQuery :: Text -> Maybe Text
parseQuery = T.stripPrefix "https://www.igdb.com/games/"

getCover :: (MonadKorrvigs m) => IGDBCredentials -> AccessToken -> Text -> IGDBId -> m (Maybe Id)
getCover creds tok title i =
  doQueryIGDB creds tok "covers" ("id=" <> T.pack (show i)) ["url"] >>= \case
    [] -> pure Nothing
    (u : _) -> do
      let url = "https:" <> u ^. coverUrl
      let covNew = NewDownloadedFile url $ def & neTitle ?~ title <> " cover"
      newFromUrl covNew

getTime :: (MonadKorrvigs m) => IGDBCredentials -> AccessToken -> Maybe Integer -> [IGDBId] -> m (Maybe ZonedTime)
getTime creds tok Nothing releases = do
  let idCond = "id=(" <> T.intercalate "," (T.pack . show <$> releases) <> ")"
  dates <- fmap (toUTC . (^. rdDate)) <$> doQueryIGDB creds tok "release_dates" idCond ["date"]
  tz <- liftIO getCurrentTimeZone
  pure $ fmap (toZT tz) $ listToMaybe $ sort dates
getTime _ _ (Just first) _ = do
  tz <- liftIO getCurrentTimeZone
  pure $ Just $ toZT tz $ toUTC first

toZT :: TimeZone -> UTCTime -> ZonedTime
toZT tz = flip ZonedTime tz . utcToLocalTime tz

toUTC :: Integer -> UTCTime
toUTC = posixSecondsToUTCTime . fromInteger

getCompanies :: (MonadKorrvigs m) => IGDBCredentials -> AccessToken -> [IGDBId] -> m ([Text], [Text])
getCompanies creds tok ids = do
  let idCond = "id=(" <> T.intercalate "," (T.pack . show <$> ids) <> ")"
  involved <- doQueryIGDB creds tok "involved_companies" idCond ["company", "developer", "publisher"]
  let nameCond = "id=(" <> T.intercalate "," (T.pack . show . (^. invCompany) <$> involved) <> ")"
  names <- doQueryIGDB creds tok "companies" nameCond ["name"]
  let nameMap = M.fromList $ (\nm -> (nm ^. compId, nm ^. compName)) <$> names
  comps <- forM involved $ \inv -> pure $ case M.lookup (inv ^. invCompany) nameMap of
    Nothing -> ([], [])
    Just nm ->
      (if inv ^. invDev then ([nm], []) else ([], []))
        <> (if inv ^. invPublisher then ([], [nm]) else ([], []))
  pure $ mconcat comps

queryIGDB :: (MonadKorrvigs m) => Text -> m (Maybe (NewEntry -> NewEntry))
queryIGDB slug = do
  let url = "https://www.igdb.com/games/" <> slug
  creds <- getCredential "igdb" >>= throwMaybe (KMiscError "No credential for IGDB")
  tok <- getAccessToken creds
  dat <- doQueryIGDB creds tok "games" ("slug=\"" <> slug <> "\"") ["cover", "first_release_date", "release_dates", "involved_companies", "name", "summary", "slug"]
  case dat of
    [] -> pure Nothing
    game : _ -> do
      let title = game ^. igdbName
      cover <- maybe (pure Nothing) (getCover creds tok title) $ game ^. igdbCover
      time <- getTime creds tok (game ^. igdbRelease) (game ^. igdbReleases)
      let day = localDay . zonedTimeToLocalTime <$> time
      liftIO $ threadDelay 1000 -- To avoid being rate limited
      (devs, pubs) <- getCompanies creds tok $ game ^. igdbCompanies
      pure $
        Just $
          foldr
            (.)
            (setMtdtValue MediaMtdt Game)
            [ setMtdtValue Abstract $ game ^. igdbSummary,
              neTitle ?~ title,
              setMtdtValue Authors devs,
              setMtdtValue Publisher pubs,
              setMtdtValue Url url,
              setMtdtValueM Cover $ unId <$> cover,
              neChildren %~ (toList cover <>),
              setMtdtValueM MedMonth $ day ^? _Just . to toGregorian . _2,
              setMtdtValueM MedYear $ day ^? _Just . to toGregorian . _1,
              maybe id (neDate ?~) day
            ]
