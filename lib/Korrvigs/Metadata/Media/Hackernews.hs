module Korrvigs.Metadata.Media.Hackernews where

import Conduit
import Control.Lens
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.URI
import Text.Parsec
import Text.Parsec.Number

data HNData = HNData
  { _hnTitle :: Text,
    _hnType :: Text,
    _hnUrl :: Text
  }

makeLenses ''HNData

instance FromJSON HNData where
  parseJSON = withObject "Hackernews Item" $ \obj ->
    HNData
      <$> obj .: "title"
      <*> obj .: "type"
      <*> obj .: "url"

type HNId = Int

hnItemUrl :: Text
hnItemUrl = "https://news.ycombinator.com/item"

parseQuery :: Text -> Maybe HNId
parseQuery url | T.isPrefixOf hnItemUrl url = do
  uri <- parseURI $ T.unpack url
  let queryP = string "?id=" >> decimal
  case runParser queryP () "" $ uriQuery uri of
    Left _ -> Nothing
    Right i -> pure i
parseQuery _ = Nothing

queryHN :: (MonadKorrvigs m) => HNId -> m (Maybe (Media, [Id]))
queryHN i = do
  let url = "https://hacker-news.firebaseio.com/v0/item/" <> T.pack (show i) <> ".json"
  req <- parseRequest $ T.unpack url
  man <- liftIO $ newManager tlsManagerSettings
  content <- runResourceT $ do
    resp <- http req man
    let scode = statusCode $ responseStatus resp
    if scode == 200
      then fmap Just $ runConduit $ responseBody resp .| sinkLazy
      else pure Nothing
  case eitherDecode <$> content of
    Just (Right hndata)
      | hndata ^. hnType == "story" ->
          let hnurl = hnItemUrl <> "?id=" <> T.pack (show i)
           in pure $
                Just
                  ( Media
                      { _medType = Blogpost,
                        _medAbstract = Nothing,
                        _medBibtex = Nothing,
                        _medDOI = [],
                        _medISBN = [],
                        _medISSN = [],
                        _medTitle = Just $ hndata ^. hnTitle,
                        _medAuthors = [],
                        _medMonth = Nothing,
                        _medYear = Nothing,
                        _medUrl = Just $ hndata ^. hnUrl,
                        _medRSS = Nothing,
                        _medSource = [],
                        _medPublisher = [],
                        _medContainer = Nothing,
                        _medInstitution = [],
                        _medLicense = [],
                        _medCover = Nothing,
                        _medDiscussion = [hnurl]
                      },
                    []
                  )
    _ -> pure Nothing
