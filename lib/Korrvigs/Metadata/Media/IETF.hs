module Korrvigs.Metadata.Media.IETF where

import Control.Lens hiding (noneOf)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Korrvigs.Utils
import Network.URI
import Text.Parsec

data IETFData = IETFData
  { _ietfName :: Text,
    _ietfPages :: Int,
    _ietfTitle :: Text,
    _ietfAbstract :: Text,
    _ietfAuthors :: [Text],
    _ietfRevHistory :: [IETFRev]
  }

data IETFRev = IETFRev
  { _ietfRevName :: Text,
    _ietfRevPublished :: ZonedTime
  }

makeLenses ''IETFData
makeLenses ''IETFRev

instance FromJSON IETFRev where
  parseJSON = withObject "IETF revision" $ \obj ->
    IETFRev
      <$> obj .: "name"
      <*> obj .: "published"

parseAuthor :: Value -> Parser Text
parseAuthor = withObject "IETF author" $ (.: "name")

instance FromJSON IETFData where
  parseJSON = withObject "IETF data" $ \obj ->
    IETFData
      <$> obj .: "name"
      <*> obj .: "pages"
      <*> obj .: "title"
      <*> obj .: "abstract"
      <*> (obj .: "authors" >>= mapM parseAuthor)
      <*> obj .: "rev_history"

type IETFId = Text

ietfUrl :: Text
ietfUrl = "https://datatracker.ietf.org/"

parseQuery :: Text -> Maybe IETFId
parseQuery url | T.isPrefixOf ietfUrl url = do
  uri <- parseURI $ T.unpack url
  case runParser urlP () "" (uriPath uri) of
    Left _ -> Nothing
    Right i -> pure i
  where
    urlP = do
      void $ string "/doc/"
      i <- manyTill (noneOf "/") $ optional (char '/') >> eof
      pure $ T.pack i
parseQuery _ = Nothing

queryIETF :: (MonadKorrvigs m) => IETFId -> m (Maybe (NewEntry -> NewEntry))
queryIETF i = do
  let url = ietfUrl <> "doc/" <> i <> "/doc.json"
  content <- simpleHttpM url
  case eitherDecode <$> content of
    Just (Right ietf) -> do
      let docUrl = ietfUrl <> "doc/" <> (ietf ^. ietfName)
      let rev = find (\r -> r ^. ietfRevName == ietf ^. ietfName) $ ietf ^. ietfRevHistory
      let date = localDay . zonedTimeToLocalTime . view ietfRevPublished <$> rev
      pure $
        Just $
          foldr
            (.)
            (setMtdtValue MediaMtdt TechReport)
            [ setMtdtValue Abstract $ ietf ^. ietfAbstract,
              neTitle ?~ (ietf ^. ietfName) <> " - " <> (ietf ^. ietfTitle),
              setMtdtValue Authors $ ietf ^. ietfAuthors,
              setMtdtValue Pages $ ietf ^. ietfPages,
              setMtdtValue Url docUrl,
              maybe id (neDate ?~) date,
              neCover .~ Nothing
            ]
    _ -> pure Nothing
