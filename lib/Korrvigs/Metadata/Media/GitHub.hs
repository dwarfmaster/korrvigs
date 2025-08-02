module Korrvigs.Metadata.Media.GitHub where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Korrvigs.Utils (reqHttpM)
import Network.HTTP.Conduit
import Network.URI
import System.FilePath

data GitHubId = GitHubId
  { _ghiOwner :: Text,
    _ghiRepo :: Text
  }

makeLenses ''GitHubId

data GitHubResponse = GitHubResponse
  { _ghrName :: Text,
    _ghrDescription :: Maybe Text,
    _ghrLicense :: Maybe Text,
    _ghrDefaultBranch :: Text
  }

makeLenses ''GitHubResponse

parseLicense :: Object -> Parser (Maybe Text)
parseLicense obj =
  obj .:? "license" >>= \case
    Nothing -> pure Nothing
    Just lic -> withObject "GitHub License" (\lobj -> Just <$> lobj .: "name") lic

instance FromJSON GitHubResponse where
  parseJSON = withObject "GitHubResponse" $ \obj ->
    GitHubResponse
      <$> obj .: "name"
      <*> obj .:? "description"
      <*> parseLicense obj
      <*> obj .: "default_branch"

parseQuery :: Text -> Maybe GitHubId
parseQuery url = do
  uri <- parseURI $ T.unpack url
  auth <- uriAuthority uri
  guard $ uriRegName auth == "github.com"
  let pth = splitPath $ uriPath uri
  case pth of
    ["/", owner, repo] ->
      pure $
        GitHubId
          { _ghiOwner = T.dropEnd 1 $ T.pack owner,
            _ghiRepo = T.pack repo
          }
    _ -> Nothing

queryGitHub :: (MonadKorrvigs m) => GitHubId -> m (Maybe (NewEntry -> NewEntry))
queryGitHub i = do
  let url = "https://api.github.com/repos/" <> i ^. ghiOwner <> "/" <> i ^. ghiRepo
  initReq <- parseRequest $ T.unpack url
  let req =
        initReq
          { requestHeaders = [("Accept", "application/vnd.github+json"), ("X-GitHub-Api-Version", "2022-11-28"), ("User-Agent", "dwarfmaster")]
          }
  content <- reqHttpM req
  case eitherDecode <$> content of
    Nothing -> pure Nothing
    Just (Left _) -> pure Nothing
    Just (Right gh) -> do
      let title = i ^. ghiRepo <> maybe "" (" - " <>) (gh ^. ghrDescription)
      pure $
        Just $
          foldr
            (.)
            (setMtdtValue MediaMtdt Software)
            [ setMtdtValue Title title,
              setMtdtValue Authors [i ^. ghiOwner],
              setMtdtValue Url $ "https://github.com/" <> i ^. ghiOwner <> "/" <> i ^. ghiRepo,
              setMtdtValue Feed $ "https://github.com/" <> i ^. ghiOwner <> "/" <> i ^. ghiRepo <> "/commits/" <> gh ^. ghrDefaultBranch <> ".atom",
              setMtdtValue Source [GitHubSource (i ^. ghiOwner) (i ^. ghiRepo)],
              setMtdtValue License $ toList $ gh ^. ghrLicense
            ]
