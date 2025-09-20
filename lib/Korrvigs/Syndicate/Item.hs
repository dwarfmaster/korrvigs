module Korrvigs.Syndicate.Item where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Entry

data SyndicatedItem = SyndicatedItem
  { _synitTitle :: Text,
    _synitUrl :: Text,
    _synitGUID :: Text,
    _synitDate :: Maybe ZonedTime,
    _synitInstance :: Maybe Id
  }

makeLenses ''SyndicatedItem

instance FromJSON SyndicatedItem where
  parseJSON = withObject "SyndicatedItem" $ \obj ->
    SyndicatedItem
      <$> obj .: "title"
      <*> obj .: "url"
      <*> obj .: "guid"
      <*> obj .:? "date"
      <*> (fmap MkId <$> obj .:? "instance")

instance ToJSON SyndicatedItem where
  toJSON syn =
    object $
      [ "title" .= (syn ^. synitTitle),
        "url" .= (syn ^. synitUrl),
        "guid" .= (syn ^. synitGUID)
      ]
        ++ maybe [] ((: []) . ("date" .=)) (syn ^. synitDate)
        ++ maybe [] ((: []) . ("instance" .=) . unId) (syn ^. synitInstance)
