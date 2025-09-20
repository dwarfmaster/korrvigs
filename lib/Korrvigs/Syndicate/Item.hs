module Korrvigs.Syndicate.Item where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Time.Clock
import Korrvigs.Entry

data SyndicatedItem = SyndicatedItem
  { _synitTitle :: Text,
    _synitUrl :: Text,
    _synitGUID :: Maybe Text,
    _synitDate :: Maybe UTCTime,
    _synitInstance :: Maybe Id
  }

makeLenses ''SyndicatedItem

instance FromJSON SyndicatedItem where
  parseJSON = withObject "SyndicatedItem" $ \obj ->
    SyndicatedItem
      <$> obj .: "title"
      <*> obj .: "url"
      <*> obj .:? "guid"
      <*> obj .:? "date"
      <*> (fmap MkId <$> obj .:? "instance")

instance ToJSON SyndicatedItem where
  toJSON syn =
    object $
      [ "title" .= (syn ^. synitTitle),
        "url" .= (syn ^. synitUrl)
      ]
        ++ maybe [] ((: []) . ("guid" .=)) (syn ^. synitGUID)
        ++ maybe [] ((: []) . ("date" .=)) (syn ^. synitDate)
        ++ maybe [] ((: []) . ("instance" .=) . unId) (syn ^. synitInstance)

isSame :: SyndicatedItem -> SyndicatedItem -> Bool
isSame it1 it2 =
  let guid1 = it1 ^. synitGUID
   in let guid2 = it2 ^. synitGUID
       in (isJust guid1 && guid1 == guid2)
            || (isNothing guid1 && isNothing guid2 && it1 ^. synitUrl == it2 ^. synitUrl)
