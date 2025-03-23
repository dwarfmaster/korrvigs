module Korrvigs.Metadata.Media.Ontology where

import Control.Arrow ((&&&))
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.List (singleton)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar

data MediaType
  = Article
  | Book
  | Booklet
  | Inbook
  | Incollection
  | Inproceedings
  | Manual
  | MastersThesis
  | PhdThesis
  | Unpublished
  | Software
  | Webcollection
  | Blogpost
  | Misc
  deriving (Show, Eq, Ord, Enum, Bounded)

data MediaSource
  = GitHubSource {_ghOwner :: Text, _ghRepo :: Text}
  | GitLabSource {_glUrl :: Text, _glOwner :: Text, _glRepo :: Text}
  | GitSource {_gitRepo :: Text}
  | VcsSource {_vcsName :: Text, _vcsSource :: Text}
  | MiscSource Text
  deriving (Show, Eq, Ord)

data MediaContainer = MediaContainer
  { _conTitle :: Text,
    _conCollection :: Maybe Text,
    _conVolume :: Maybe Int,
    _conChapter :: Maybe Text,
    _conPages :: Maybe (Int, Int)
  }
  deriving (Show, Eq, Ord)

data Media = Media
  { _medType :: MediaType,
    _medAbstract :: Maybe Text,
    _medBibtex :: Maybe Text,
    _medDOI :: [Text],
    _medISBN :: [Text],
    _medISSN :: [Text],
    _medTitle :: Maybe Text,
    _medAuthors :: [Text],
    _medMonth :: Maybe MonthOfYear,
    _medYear :: Maybe Year,
    _medUrl :: Maybe Text,
    _medRSS :: Maybe Text,
    _medSource :: [MediaSource],
    _medPublisher :: [Text],
    _medContainer :: Maybe MediaContainer,
    _medInstitution :: [Text],
    _medLicense :: [Text]
  }
  deriving (Show, Eq, Ord)

makeLenses ''MediaSource
makeLenses ''MediaContainer
makeLenses ''Media

displayMediaType :: MediaType -> Text
displayMediaType Article = "article"
displayMediaType Book = "book"
displayMediaType Booklet = "booklet"
displayMediaType Inbook = "inbook"
displayMediaType Incollection = "incollection"
displayMediaType Inproceedings = "inproceedings"
displayMediaType Manual = "manual"
displayMediaType MastersThesis = "mastersthesis"
displayMediaType PhdThesis = "phdthesis"
displayMediaType Unpublished = "unpublished"
displayMediaType Software = "software"
displayMediaType Webcollection = "webcollection"
displayMediaType Blogpost = "blogpost"
displayMediaType Misc = "misc"

mediaTypeMap :: Map Text MediaType
mediaTypeMap = M.fromList $ (displayMediaType &&& id) <$> [minBound .. maxBound]

instance ToJSON MediaType where
  toJSON = String . displayMediaType

instance FromJSON MediaType where
  parseJSON = withText "MediaType" $ \txt -> case M.lookup txt mediaTypeMap of
    Just md -> pure md
    Nothing -> fail $ T.unpack $ "\"" <> txt <> "\" is not a valid media type"

instance ToJSON MediaSource where
  toJSON (GitHubSource owner repo) =
    object
      [ "source" .= ("github" :: Text),
        "owner" .= owner,
        "repo" .= repo
      ]
  toJSON (GitLabSource url owner repo) =
    object
      [ "source" .= ("gitlab" :: Text),
        "url" .= url,
        "owner" .= owner,
        "repo" .= repo
      ]
  toJSON (GitSource url) =
    object
      [ "source" .= ("git" :: Text),
        "url" .= url
      ]
  toJSON (VcsSource vcs url) =
    object
      [ "source" .= ("vcs" :: Text),
        "vcs" .= vcs,
        "url" .= url
      ]
  toJSON (MiscSource url) =
    object
      [ "source" .= ("misc" :: Text),
        "url" .= url
      ]

instance FromJSON MediaSource where
  parseJSON = withObject "MediaSource" $ \obj ->
    obj .: "source" >>= \case
      "github" -> GitHubSource <$> obj .: "owner" <*> obj .: "repo"
      "gitlab" -> GitLabSource <$> obj .: "url" <*> obj .: "owner" <*> obj .: "repo"
      "git" -> GitSource <$> obj .: "url"
      "vcs" -> VcsSource <$> obj .: "vcs" <*> obj .: "url"
      "misc" -> MiscSource <$> obj .: "url"
      src -> fail $ T.unpack $ "\"" <> src <> "\" is not a valid media source"

instance ToJSON MediaContainer where
  toJSON (MediaContainer title col vol chapter pages) =
    object $
      ["title" .= title]
        ++ maybe [] (singleton . ("collection" .=)) col
        ++ maybe [] (singleton . ("volume" .=)) vol
        ++ maybe [] (singleton . ("chapter" .=)) chapter
        ++ maybe [] (\(s, e) -> ["pages" .= object ["start" .= s, "end" .= e]]) pages

instance FromJSON MediaContainer where
  parseJSON = withObject "MediaContainer" $ \obj ->
    MediaContainer
      <$> obj .: "title"
      <*> obj .:? "collection"
      <*> obj .:? "volume"
      <*> obj .:? "chapter"
      <*> (obj .:? "page" >>= maybe (pure Nothing) (fmap Just . parseJSONPages))

instance Default MediaContainer where
  def = MediaContainer "" Nothing Nothing Nothing Nothing

parseJSONPages :: Value -> Parser (Int, Int)
parseJSONPages = withObject "Book pages" $ \obj ->
  (,) <$> obj .: "start" <*> obj .: "end"
