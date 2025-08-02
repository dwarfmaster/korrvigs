module Korrvigs.Metadata.Media.Steam where

import Citeproc.Types (readAsInt)
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Default
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.Download
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Korrvigs.Utils
import Network.URI
import System.FilePath

data SteamData = SteamData
  { _stName :: Text,
    _stDescription :: Text,
    _stImage :: Maybe Text,
    _stWebsite :: Maybe Text,
    _stDevs :: [Text],
    _stPublishers :: [Text]
  }

makeLenses ''SteamData

instance FromJSON SteamData where
  parseJSON = withObject "SteamResponse" $ \resp -> do
    dat <- resp .: "data"
    withObject
      "SteamData"
      ( \obj ->
          SteamData
            <$> (obj .: "name")
            <*> (obj .: "short_description")
            <*> (obj .:? "header_image")
            <*> (obj .:? "website")
            <*> (obj .:? "developers" <&> fromMaybe [])
            <*> (obj .:? "publishers" <&> fromMaybe [])
      )
      dat

type SteamID = Int

parseQuery :: Text -> Maybe SteamID
parseQuery url | T.isPrefixOf steamUrl url = do
  uri <- parseURI $ T.unpack url
  key <- splitPath (uriPath uri) ^? ix 2
  readAsInt $ T.init $ T.pack key
parseQuery _ = Nothing

steamUrl :: Text
steamUrl = "https://store.steampowered.com"

querySteam :: (MonadKorrvigs m) => SteamID -> m (Maybe (NewEntry -> NewEntry))
querySteam i = do
  let url = steamUrl <> "/api/appdetails?appids=" <> T.pack (show i)
  content <- simpleHttpM url
  case extract . eitherDecode <$> content of
    Nothing -> pure Nothing
    Just [] -> pure Nothing
    Just (steam : _) -> do
      let title = steam ^. stName
      let steamGameUrl = steamUrl <> "/app/" <> T.pack (show i)
      let gameUrl = fromMaybe steamGameUrl $ steam ^. stWebsite
      let rssUrl = steamUrl <> "/feeds/news/app/" <> T.pack (show i)
      dlCover <- fmap join $ forM (steam ^. stImage) $ \img -> do
        let imgNew = NewDownloadedFile img $ def & neTitle ?~ title <> " cover"
        newFromUrl imgNew
      let forum = "https://steamcommunity.com/app/" <> T.pack (show i) <> "/discussions"
      pure $
        Just $
          foldr
            (.)
            (setMtdtValue MediaMtdt Game)
            [ setMtdtValue Abstract $ steam ^. stDescription,
              setMtdtValue Title title,
              setMtdtValue Authors $ steam ^. stDevs,
              setMtdtValue Url gameUrl,
              setMtdtValue Feed rssUrl,
              setMtdtValue Publisher $ steam ^. stPublishers,
              setMtdtValueM Cover $ unId <$> dlCover,
              setMtdtValue Discussions [forum],
              neChildren %~ (toList dlCover <>)
            ]
  where
    extract :: Either a (Map Text c) -> [c]
    extract (Left _) = []
    extract (Right mp) = M.elems mp
