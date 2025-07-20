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
import Control.Arrow (first)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.CaseInsensitive as CI
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
import qualified Korrvigs.Metadata.Media.MangaUpdates as MU
import Korrvigs.Metadata.Media.Ontology
import qualified Korrvigs.Metadata.Media.OpenLibrary as OL
import qualified Korrvigs.Metadata.Media.Pandoc as Pd
import qualified Korrvigs.Metadata.Media.Steam as Steam
import qualified Korrvigs.Metadata.Media.Trivial as Trivial
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Monad.Collections (capture)
import Korrvigs.Monad.Metadata (updateParents)
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
  = DispatcherSuccess (Media, [Id])
  | DispatcherSkip
  | DispatcherFail Text

instance Semigroup DispatcherData where
  DispatcherSkip <> m = m
  DispatcherFail lbl <> _ = DispatcherFail lbl
  (DispatcherSuccess x) <> _ = DispatcherSuccess x

instance Monoid DispatcherData where
  mempty = DispatcherSkip

mkDispatcher :: (MonadKorrvigs m) => Text -> (Text -> m (Maybe a)) -> (a -> m (Maybe (Media, [Id]))) -> Text -> m DispatcherData
mkDispatcher lbl parser extractor txt =
  parser txt >>= \case
    Nothing -> pure DispatcherSkip
    Just parsed ->
      extractor parsed >>= \case
        Nothing -> pure $ DispatcherFail lbl
        Just med -> pure $ DispatcherSuccess med

mkDispatcherIO :: (MonadKorrvigs m) => Text -> (Text -> IO (Maybe a)) -> (a -> IO (Maybe Media)) -> Text -> m DispatcherData
mkDispatcherIO lbl parser extractor =
  mkDispatcher lbl (liftIO . parser) (liftIO . fmap (fmap (,[])) . extractor)

dispatchMedia :: (MonadKorrvigs m) => NewMedia -> m (Media, [Id])
dispatchMedia nm = do
  dispatch <- sequence dispatchers
  case fold dispatch of
    DispatcherSuccess md -> pure md
    DispatcherSkip ->
      pure
        ( Media
            { _medType = fromMaybe Blogpost $ nm ^. nmType,
              _medAbstract = Nothing,
              _medBibtex = Nothing,
              _medDOI = [],
              _medISBN = [],
              _medISSN = [],
              _medTitle = Nothing,
              _medAuthors = [],
              _medMonth = Nothing,
              _medYear = Nothing,
              _medUrl = Just $ nm ^. nmInput,
              _medRSS = Nothing,
              _medSource = [],
              _medPublisher = [],
              _medContainer = Nothing,
              _medInstitution = [],
              _medLicense = [],
              _medCover = Nothing,
              _medDiscussion = []
            },
          []
        )
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
              mkDispatcher "Trivial" (pure . Trivial.parseQuery) Trivial.query,
              mkDispatcherIO "BibTeX/RIS" Pd.importRef (pure . Just)
            ]

mergeInto :: Media -> NewEntry -> NewEntry
mergeInto md =
  maybe id (neTitle ?~) (md ^. medTitle)
    . (neMtdt %~ ((first CI.foldedCase <$> M.toList (M.delete (mtdtName Title) $ mediaMetadata md)) ++))

prepareNewMedia :: (MonadKorrvigs m) => NewMedia -> m (NewMediaInternal, [Id])
prepareNewMedia nm = do
  (md, subs) <- dispatchMedia nm
  let ne =
        mergeInto md (nm ^. nmEntry)
          & neMtdt %~ ((mtdtSqlName TaskMtdt, "todo") :)
  let title = fromMaybe (medTxt (md ^. medType) <> " " <> nm ^. nmInput) $ ne ^. neTitle
  let url = fromMaybe (nm ^. nmInput) $ md ^. medUrl
  let nlink = NewLinkMedia url $ Link.NewLink ne False
  pure . (,subs) $ case md ^. medType of
    Blogpost -> nlink
    Video -> nlink
    _ -> NewNoteMedia $ Note.NewNote ne title
  where
    medTxt :: MediaType -> Text
    medTxt Article = "Article"
    medTxt Book = "Livre"
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
    medTxt Game = "Game"
    medTxt Movie = "Movie"
    medTxt Video = "Video"
    medTxt Song = "Song"
    medTxt Webcomic = "Webcomic"
    medTxt Blog = "Blog"
    medTxt Podcast = "Podcast"
    medTxt Album = "Album"
    medTxt Channel = "Channel"
    medTxt Misc = "Misc"

new :: (MonadKorrvigs m) => NewMedia -> m Id
new nm = do
  (nmed, subs) <- prepareNewMedia nm
  i <- case nmed of
    NewLinkMedia url nl -> Link.new url nl
    NewNoteMedia nn -> Note.new nn
  void $ capture i
  forM_ subs $
    load >=> \case
      Nothing -> pure ()
      Just subEntry -> updateParents subEntry [i] []
  pure i
