module Korrvigs.Metadata.Media.New
  ( NewMedia (..),
    nmEntry,
    nmInput,
    nmType,
    nmCapture,
    new,
  )
where

import Conduit (throwM)
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Entry.New
import qualified Korrvigs.Link.New as Link
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import qualified Korrvigs.Metadata.Media.Arxiv as AR
import qualified Korrvigs.Metadata.Media.GitHub as GH
import qualified Korrvigs.Metadata.Media.Hackernews as HN
import qualified Korrvigs.Metadata.Media.IGDB as IGDB
import qualified Korrvigs.Metadata.Media.MangaUpdates as MU
import qualified Korrvigs.Metadata.Media.MusicBrainz as MB
import qualified Korrvigs.Metadata.Media.OMDB as OMDB
import Korrvigs.Metadata.Media.Ontology
import qualified Korrvigs.Metadata.Media.OpenLibrary as OL
import qualified Korrvigs.Metadata.Media.Pandoc as Pd
import qualified Korrvigs.Metadata.Media.Steam as Steam
import qualified Korrvigs.Metadata.Media.Youtube as Yt
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import qualified Korrvigs.Note.New as Note

data NewMedia = NewMedia
  { _nmEntry :: NewEntry,
    _nmInput :: Text,
    _nmType :: Maybe MediaType,
    _nmCapture :: Bool
  }

makeLenses ''NewMedia

data NewMediaInternal
  = NewLinkMedia Text Link.NewLink
  | NewNoteMedia Note.NewNote

makeLenses ''NewMediaInternal

data DispatcherData
  = DispatcherSuccess (NewEntry -> NewEntry)
  | DispatcherSkip
  | DispatcherFail Text

instance Semigroup DispatcherData where
  DispatcherSkip <> m = m
  DispatcherFail lbl <> _ = DispatcherFail lbl
  (DispatcherSuccess x) <> _ = DispatcherSuccess x

instance Monoid DispatcherData where
  mempty = DispatcherSkip

mkDispatcher :: (MonadKorrvigs m) => Text -> (Text -> m (Maybe a)) -> (a -> m (Maybe (NewEntry -> NewEntry))) -> Text -> m DispatcherData
mkDispatcher lbl parser extractor txt =
  parser txt >>= \case
    Nothing -> pure DispatcherSkip
    Just parsed ->
      extractor parsed >>= \case
        Nothing -> pure $ DispatcherFail lbl
        Just med -> pure $ DispatcherSuccess med

mkDispatcherIO :: (MonadKorrvigs m) => Text -> (Text -> IO (Maybe a)) -> (a -> IO (Maybe (NewEntry -> NewEntry))) -> Text -> m DispatcherData
mkDispatcherIO lbl parser extractor =
  mkDispatcher lbl (liftIO . parser) (liftIO . extractor)

dispatchMedia :: (MonadKorrvigs m) => NewMedia -> m (NewEntry -> NewEntry)
dispatchMedia nm = do
  dispatch <- sequence dispatchers
  case fold dispatch of
    DispatcherSuccess md -> pure md
    DispatcherSkip ->
      pure $
        setMtdtValue MediaMtdt (fromMaybe Blogpost $ nm ^. nmType)
          . setMtdtValue Url (nm ^. nmInput)
    DispatcherFail lbl -> throwM $ KMiscError $ "Failed to import from " <> lbl
  where
    dispatchers =
      ($ (nm ^. nmInput))
        <$> [ mkDispatcher "OpenLibrary" (pure . OL.parseQuery) OL.queryOpenLibrary,
              mkDispatcher "MangaUpdates" (pure . MU.isMangaUpdates) MU.queryMangaUpdates,
              mkDispatcher "Arxiv" (pure . AR.parseQuery) AR.queryArxiv,
              mkDispatcher "Steam" (pure . Steam.parseQuery) Steam.querySteam,
              mkDispatcher "GitHub" (pure . GH.parseQuery) GH.queryGitHub,
              mkDispatcher "Hackernews" (pure . HN.parseQuery) HN.queryHN,
              mkDispatcher "OMDB" (pure . OMDB.parseQuery) OMDB.queryOMDB,
              mkDispatcher "IGDB" (pure . IGDB.parseQuery) IGDB.queryIGDB,
              mkDispatcher "MusicBrainz" (pure . MB.parseQuery) MB.queryMB,
              mkDispatcher "Youtube" (pure . Yt.parseQuery) Yt.queryYoutube,
              mkDispatcherIO "BibTeX/RIS" Pd.importRef (pure . Just)
            ]

prepareNewMedia :: (MonadKorrvigs m) => NewMedia -> m NewMediaInternal
prepareNewMedia nm = do
  md <- dispatchMedia nm
  let ne =
        md (nm ^. nmEntry)
          & neMtdt %~ M.insert (mtdtName TaskMtdt) "todo"
  let tp = fromMaybe Blogpost $ ne ^? neMtdt . at (mtdtName MediaMtdt) . _Just . _JSON
  let title = fromMaybe (medTxt tp <> " " <> nm ^. nmInput) $ ne ^. neTitle
  let url = fromMaybe (nm ^. nmInput) $ ne ^? neMtdt . at (mtdtName Url) . _Just . _JSON
  let nlink = NewLinkMedia url $ Link.NewLink ne False
  pure $
    if mediaTypeDefaultToNote tp
      then NewNoteMedia $ Note.NewNote ne title (isNothing $ ne ^. neTitle) False
      else nlink
  where
    medTxt :: MediaType -> Text
    medTxt Article = "Article"
    medTxt Book = "Livre"
    medTxt Comic = "BD"
    medTxt Booklet = "Brochure"
    medTxt Inbook = "Chapitre"
    medTxt Incollection = "Partie"
    medTxt Inproceedings = "Proceedings"
    medTxt Manual = "Manuel"
    medTxt MastersThesis = "Thèse de master"
    medTxt PhdThesis = "Manuscript"
    medTxt Unpublished = "Article"
    medTxt Software = "Logiciel"
    medTxt Webcollection = "Collection"
    medTxt Blogpost = "Poste de blog"
    medTxt Manga = "Manga"
    medTxt Chapter = "Chapitre"
    medTxt Page = "Page"
    medTxt Game = "Game"
    medTxt Movie = "Movie"
    medTxt Episode = "Épisode"
    medTxt Video = "Video"
    medTxt Song = "Song"
    medTxt Webcomic = "Webcomic"
    medTxt Blog = "Blog"
    medTxt Podcast = "Podcast"
    medTxt Album = "Album"
    medTxt Channel = "Channel"
    medTxt Show = "Série"
    medTxt Misc = "Misc"

new :: (MonadKorrvigs m) => NewMedia -> m Id
new nm = do
  nmed <- prepareNewMedia nm
  case nmed of
    NewLinkMedia url nl -> Link.new url nl
    NewNoteMedia nn -> Note.new nn
