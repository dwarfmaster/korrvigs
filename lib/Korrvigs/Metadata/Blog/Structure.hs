module Korrvigs.Metadata.Blog.Structure where

import Conduit
import Control.Applicative ((<|>))
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Blog.Mtdt
import Korrvigs.Monad
import Korrvigs.Note.SQL
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye
import Opaleye
import qualified Opaleye as O

data BlogConfig = BlogConfig
  { _blogCfgUrl :: Text,
    _blogCfgNote :: Id,
    _blogCfgOnlyPublished :: Bool
  }
  deriving (Eq, Ord, Show)

data BlogUrl
  = BlogTopLevel Text
  | BlogFilePlain Text
  | BlogPostNote Text
  | BlogComputation Text Text RunnableType
  | BlogArchive
  | BlogArchiveTag Text
  | BlogAtom
  | BlogAtomTag Text
  deriving (Eq, Ord, Show)

data BlogContent
  = BlogFromNote Id
  | BlogFromFile Id
  | BlogFromCode Id Text
  | BlogFromComp Id Text
  | BlogFromArchive
  | BlogFromArchiveTag Text
  | BlogFromAtom
  | BlogFromAtomTag Text
  deriving (Eq, Ord, Show)

data BlogMenuContent = BlogMenuContent
  { _blogMenuTitle :: Text,
    _blogMenuItems :: [(Text, Text)]
  }
  deriving (Eq, Show)

data BlogStructure = BlogStructure
  { _blogMtdt :: Map Text Text,
    _blogFiles :: Map BlogUrl BlogContent,
    _blogTags :: [Text],
    _blogMenu :: BlogMenuContent,
    _blogCSL :: (Id, Text)
  }
  deriving (Eq, Show)

makeLenses ''BlogConfig
makeLenses ''BlogMenuContent
makeLenses ''BlogStructure

loadMtdt :: (MonadKorrvigs m) => BlogConfig -> m (Map Text Text)
loadMtdt cfg = do
  mtdt <- rSelectOne $ fromName (baseSelectMtdt BlogMtdt) $ sqlId $ cfg ^. blogCfgNote
  let m = fromMaybe M.empty $ mtdt >>= fromJSONM
  pure $ m <> common
  where
    common =
      M.fromList
        [ ("viewport", "width=device-width, initial-scale=1.0"),
          ("language", "EN"),
          ("url", cfg ^. blogCfgUrl),
          ("identifier-URL", cfg ^. blogCfgUrl),
          ("application-name", "Korrvigs blog"),
          ("publisher", "Korrvigs")
        ]

loadFiles :: (MonadKorrvigs m) => BlogConfig -> m (Map BlogUrl BlogContent)
loadFiles cfg = do
  topLevelsSQL <- rSelectOne $ fromName (baseSelectMtdt BlogFiles) $ sqlId $ cfg ^. blogCfgNote
  let topLevels =
        M.fromList $
          fmap (BlogTopLevel *** parseBlogTopContent cfg) $
            M.toList $
              fromMaybe M.empty $
                topLevelsSQL >>= fromJSONM
  filesSQL <- rSelect $ do
    entry <- selectTable entriesTable
    file <- baseSelectTextMtdt BlogFile $ entry ^. sqlEntryId
    pure (file, entry ^. sqlEntryName)
  let files = M.fromList $ (BlogFilePlain *** BlogFromFile) <$> filesSQL
  let selectPostEntries = do
        entry <- selectTable entriesTable
        where_ $ entry ^. sqlEntryKind .== sqlKind Note
        post <- baseSelectTextMtdt BlogPost $ entry ^. sqlEntryId
        when (cfg ^. blogCfgOnlyPublished) $
          where_ $
            O.not $
              isNull $
                entry ^. sqlEntryDate
        pure (post, entry)
  postsSQL <- rSelect $ do
    (post, entry) <- selectPostEntries
    pure (post, entry ^. sqlEntryName)
  let posts = M.fromList $ (BlogPostNote *** BlogFromNote) <$> postsSQL
  compsSQL <- rSelect $ do
    (post, entry) <- selectPostEntries
    cmp <- selectTable computationsTable
    where_ $ cmp ^. sqlCompEntry .== (entry ^. sqlEntryId)
    pure (post, cmp ^. sqlCompName, cmp ^. sqlCompType)
  let comps = (\(post, cmp, tp) -> (BlogComputation post cmp tp, BlogFromComp (MkId post) cmp)) <$> compsSQL
  pure $ topLevels <> files <> posts <> M.fromList comps

parseBlogTopContent :: BlogConfig -> Text -> BlogContent
parseBlogTopContent cfg targetId = case T.split (== '#') targetId of
  ["", code] -> BlogFromCode (cfg ^. blogCfgNote) code
  [i, code] -> BlogFromCode (MkId i) code
  _ -> BlogFromNote $ MkId targetId

loadTags :: (MonadKorrvigs m) => BlogConfig -> m [Text]
loadTags cfg = do
  rSelect $ distinct $ orderBy (asc id) $ do
    mtdt <- selectTable entriesMetadataTable
    where_ $ mtdt ^. sqlKey .== sqlStrictText (mtdtSqlName BlogTags)
    when (cfg ^. blogCfgOnlyPublished) $ do
      entry <- selectTable entriesTable
      where_ $ entry ^. sqlEntryId .== (mtdt ^. sqlEntry)
      where_ $ O.not $ isNull $ entry ^. sqlEntryDate
    sqlJsonElementsText $ toNullable $ mtdt ^. sqlValue

parsePub :: Text -> Maybe Day
parsePub = parseTimeM True defaultTimeLocale "%F" . T.unpack

selectBlogTitle :: EntryRowSQLR -> Select (Field SqlText)
selectBlogTitle entry = do
  btitle <- selectTextMtdt BlogTitle $ entry ^. sqlEntryId
  title <- fromNullableSelect $ pure $ entry ^. sqlEntryTitle
  pure $ fromNullable title btitle

loadForTag :: (MonadKorrvigs m) => Bool -> Maybe Text -> Maybe Int -> m [(Text, Day, Text, FilePath)]
loadForTag onlyPublished mtag mlimit = do
  time <- liftIO getCurrentTime
  tags <- rSelect $ applyLimit $ orderBy (desc (view _2) <> asc (view _1)) $ do
    entry <- selectTable entriesTable
    note <- selectTable notesTable
    where_ $ note ^. sqlNoteId .== entry ^. sqlEntryId
    post <- baseSelectTextMtdt BlogPost $ entry ^. sqlEntryId
    title <- selectBlogTitle entry
    pub <-
      if onlyPublished
        then fromNullableSelect $ pure $ entry ^. sqlEntryDate
        else do
          let mpub = entry ^. sqlEntryDate
          pure $ fromNullable (sqlUTCTime time) mpub
    case mtag of
      Nothing -> pure ()
      Just tag -> limit 1 $ do
        tags <- baseSelectMtdt BlogTags $ entry ^. sqlEntryId
        sqlTag <- sqlJsonElementsText $ toNullable tags
        where_ $ sqlStrictText tag .== sqlTag
    pure (post, pub, title, note ^. sqlNotePath)
  pure $ prepTag <$> tags
  where
    prepTag :: (Text, UTCTime, Text, FilePath) -> (Text, Day, Text, FilePath)
    prepTag = _2 %~ utctDay
    applyLimit :: Select a -> Select a
    applyLimit = case mlimit of
      Nothing -> id
      Just lim -> limit lim

loadArchivesAndAtoms :: (MonadKorrvigs m) => [Text] -> m (Map BlogUrl BlogContent)
loadArchivesAndAtoms tags = do
  let toplevel = M.fromList [(BlogArchive, BlogFromArchive), (BlogAtom, BlogFromAtom)]
  let archives = M.fromList $ (BlogArchiveTag &&& BlogFromArchiveTag) <$> tags
  let atoms = M.fromList $ (BlogAtomTag &&& BlogFromAtomTag) <$> tags
  pure $ toplevel <> archives <> atoms

loadMenu :: (MonadKorrvigs m) => BlogConfig -> m BlogMenuContent
loadMenu cfg = do
  let i = cfg ^. blogCfgNote
  blogentry <- load i >>= throwMaybe (KCantLoad i "Failed to load note for blog")
  blogtitle <- rSelectMtdt BlogTitle $ sqlId i
  let title = fromMaybe "Blog" $ blogtitle <|> (blogentry ^. entryTitle)
  content <- rSelectMtdt BlogMenu $ sqlId i
  pure $
    BlogMenuContent
      { _blogMenuTitle = title,
        _blogMenuItems = fromMaybe [] content
      }

loadCSL :: (MonadKorrvigs m) => BlogConfig -> m (Id, Text)
loadCSL cfg = do
  cslMtdt <- rSelectMtdt BlogCSL $ sqlId $ cfg ^. blogCfgNote
  case T.split (== '#') <$> cslMtdt of
    Just [i, code] -> pure (MkId i, code)
    Just [code] -> pure (cfg ^. blogCfgNote, code)
    Just _ -> throwM $ KMiscError $ "Invalid CSL for blog " <> unId (cfg ^. blogCfgNote)
    Nothing -> throwM $ KMiscError $ "CSL undefined for blog " <> unId (cfg ^. blogCfgNote)

loadStructure :: (MonadKorrvigs m) => BlogConfig -> m BlogStructure
loadStructure cfg = do
  mtdt <- loadMtdt cfg
  files <- loadFiles cfg
  tags <- loadTags cfg
  tagFiles <- loadArchivesAndAtoms tags
  menuContent <- loadMenu cfg
  csl <- loadCSL cfg
  pure $ BlogStructure mtdt (files <> tagFiles) tags menuContent csl
