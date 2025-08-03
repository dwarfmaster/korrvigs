module Korrvigs.Metadata.Media.Hackernews where

import Control.Lens
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry.New
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Korrvigs.Utils
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

queryHN :: (MonadKorrvigs m) => HNId -> m (Maybe (NewEntry -> NewEntry))
queryHN i = do
  let url = "https://hacker-news.firebaseio.com/v0/item/" <> T.pack (show i) <> ".json"
  content <- simpleHttpM url
  case eitherDecode <$> content of
    Just (Right hndata)
      | hndata ^. hnType == "story" ->
          let hnurl = hnItemUrl <> "?id=" <> T.pack (show i)
           in pure $
                Just $
                  foldr
                    (.)
                    (setMtdtValue MediaMtdt Blogpost)
                    [ neTitle ?~ hndata ^. hnTitle,
                      setMtdtValue Url hnurl,
                      setMtdtValue Discussions [hnurl]
                    ]
    _ -> pure Nothing
