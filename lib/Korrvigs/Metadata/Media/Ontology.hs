module Korrvigs.Metadata.Media.Ontology where

import Control.Arrow ((&&&))
import Control.Lens hiding ((.=))
import Data.Aeson
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

data MediaInBook = MediaInBook
  { _bookName :: Text,
    _bookChapter :: Text,
    _bookPage :: Int
  }
  deriving (Show, Eq, Ord)

data Media = Media
  { _medType :: MediaType,
    _medDOI :: [Text],
    _medISBN :: [Text],
    _medTitle :: Maybe Text,
    _medAuthors :: [Text],
    _medYear :: Maybe Year,
    _medUrl :: Maybe Text,
    _medRSS :: Maybe Text,
    _medSource :: [MediaSource],
    _medJournal :: [Text],
    _medPublisher :: [Text],
    _medInBook :: Maybe MediaInBook,
    _medInCollection :: Maybe Text,
    _medInstitution :: [Text],
    _medLicense :: [Text]
  }
  deriving (Show, Eq, Ord)

makeLenses ''MediaSource
makeLenses ''MediaInBook
makeLenses ''Media

displayMediaType :: MediaType -> Text
displayMediaType Article = "article"
displayMediaType Book = "book"
displayMediaType Booklet = "booklet"
displayMediaType Inbook = "inbook"
displayMediaType Incollection = "incollection"
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

instance ToJSON MediaInBook where
  toJSON (MediaInBook book chapter page) =
    object
      [ "book" .= book,
        "chapter" .= chapter,
        "page" .= page
      ]

instance FromJSON MediaInBook where
  parseJSON = withObject "MediaInBook" $ \obj ->
    MediaInBook <$> obj .: "book" <*> obj .: "chapter" <*> obj .: "page"
