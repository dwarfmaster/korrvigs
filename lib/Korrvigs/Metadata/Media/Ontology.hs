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

data MediaType
  = Article
  | Book
  | Comic
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
  | Manga
  | Chapter
  | Page
  | Game
  | Movie
  | Episode
  | Video
  | Song
  | Webcomic
  | Blog
  | Podcast
  | Album
  | Channel
  | Show
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

makeLenses ''MediaSource
makeLenses ''MediaContainer

displayMediaType :: MediaType -> Text
displayMediaType Article = "article"
displayMediaType Book = "book"
displayMediaType Comic = "comic"
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
displayMediaType Chapter = "chapter"
displayMediaType Page = "page"
displayMediaType Manga = "manga"
displayMediaType Game = "game"
displayMediaType Movie = "movie"
displayMediaType Episode = "episode"
displayMediaType Video = "video"
displayMediaType Song = "song"
displayMediaType Webcomic = "webcomic"
displayMediaType Blog = "blog"
displayMediaType Podcast = "podcast"
displayMediaType Album = "album"
displayMediaType Channel = "channel"
displayMediaType Show = "show"
displayMediaType Misc = "misc"

mediaTypeDefaultToNote :: MediaType -> Bool
mediaTypeDefaultToNote Blogpost = False
mediaTypeDefaultToNote Chapter = False
mediaTypeDefaultToNote Page = False
mediaTypeDefaultToNote Episode = False
mediaTypeDefaultToNote Video = False
mediaTypeDefaultToNote Song = False
mediaTypeDefaultToNote _ = True

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
