module Korrvigs.Metadata.Media.New
  ( NewMedia (..),
    nmEntry,
    nmUrl,
    nmType,
    new,
  )
where

import Control.Arrow (first)
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.CaseInsensitive as CI
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Entry.New
import qualified Korrvigs.Link.New as Link
import Korrvigs.Metadata
import Korrvigs.Metadata.Collections
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import qualified Korrvigs.Metadata.Media.OpenLibrary as OL
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import qualified Korrvigs.Note.New as Note

data NewMedia = NewMedia
  { _nmEntry :: NewEntry,
    _nmUrl :: Text,
    _nmType :: Maybe MediaType
  }

makeLenses ''NewMedia

data NewMediaInternal
  = NewLinkMedia Text Link.NewLink
  | NewNoteMedia Note.NewNote

makeLenses ''NewMediaInternal

data DispatcherData
  = DispatcherSuccess Media
  | DispatcherSkip
  | DispatcherFail

instance Semigroup DispatcherData where
  DispatcherSkip <> m = m
  DispatcherFail <> _ = DispatcherFail
  (DispatcherSuccess x) <> _ = DispatcherSuccess x

instance Monoid DispatcherData where
  mempty = DispatcherSkip

mkDispatcher :: (Text -> Maybe a) -> (a -> IO (Maybe Media)) -> Text -> IO DispatcherData
mkDispatcher parser extractor txt = case parser txt of
  Nothing -> pure DispatcherSkip
  Just parsed ->
    extractor parsed >>= \case
      Nothing -> pure DispatcherFail
      Just med -> pure $ DispatcherSuccess med

dispatchMedia :: (MonadKorrvigs m) => NewMedia -> m Media
dispatchMedia nm = do
  dispatch <- liftIO $ sequence dispatchers
  case fold dispatch of
    DispatcherSuccess md -> pure md
    _ ->
      pure $
        Media
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
            _medUrl = Just $ nm ^. nmUrl,
            _medRSS = Nothing,
            _medSource = [],
            _medPublisher = [],
            _medContainer = Nothing,
            _medInstitution = [],
            _medLicense = []
          }
  where
    dispatchers =
      ($ (nm ^. nmUrl))
        <$> [ mkDispatcher OL.parseQuery OL.queryOpenLibrary
            ]

mergeInto :: Media -> NewEntry -> NewEntry
mergeInto md =
  maybe id (neTitle ?~) (md ^. medTitle)
    . (neMtdt %~ ((first CI.foldedCase <$> M.toList (M.delete (mtdtName Title) $ mediaMetadata md)) ++))

insertCollection :: [Text] -> [(Text, Value)] -> [(Text, Value)]
insertCollection col mtdts = case find (\m -> CI.mk (fst m) == mtdtName TaskSet) mtdts of
  Nothing -> (mtdtSqlName TaskSet, toJSON [col]) : mtdts
  Just (_, v) -> case fromJSON v of
    Error _ -> (mtdtSqlName TaskSet, toJSON [col]) : mtdts'
    Success cols -> (mtdtSqlName TaskSet, toJSON $ col : cols) : mtdts'
  where
    mtdts' = filter ((/= mtdtName TaskSet) . CI.mk . fst) mtdts

prepareNewMedia :: (MonadKorrvigs m) => NewMedia -> m NewMediaInternal
prepareNewMedia nm = do
  md <- dispatchMedia nm
  let ne =
        mergeInto md (nm ^. nmEntry)
          & neMtdt %~ ((mtdtSqlName TaskMtdt, "todo") :)
          & neMtdt %~ insertCollection ["Captured"]
  let title = fromMaybe (medTxt (md ^. medType) <> " " <> nm ^. nmUrl) $ ne ^. neTitle
  case md ^. medType of
    Blogpost -> pure $ NewLinkMedia (nm ^. nmUrl) $ Link.NewLink ne False
    _ -> pure $ NewNoteMedia $ Note.NewNote ne title
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
    medTxt Misc = "Misc"

new :: (MonadKorrvigs m) => NewMedia -> m Id
new nm =
  prepareNewMedia nm >>= \case
    NewLinkMedia url nl -> Link.new url nl
    NewNoteMedia nn -> Note.new nn
