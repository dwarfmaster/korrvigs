module Korrvigs.Metadata.Blog.Structure where

import Control.Arrow
import Control.Lens
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
import Korrvigs.Utils.JSON
import Opaleye

data BlogConfig = BlogConfig
  { _blogCfgUrl :: Text,
    _blogCfgNote :: Id
  }
  deriving (Eq, Ord, Show)

data BlogUrl
  = BlogTopLevel Text
  | BlogFilePlain Text
  | BlogPostNote Text
  | BlogComputation Text Text RunnableType
  deriving (Eq, Ord, Show)

data BlogContent
  = BlogFromNote Id
  | BlogFromFile Id
  | BlogFromCode Id Text
  deriving (Eq, Ord, Show)

data BlogStructure = BlogStructure
  { _blogMtdt :: Map Text Text,
    _blogFiles :: Map BlogUrl BlogContent,
    _blogEntries :: Map Id (BlogUrl, Maybe Day)
  }
  deriving (Eq, Show)

makeLenses ''BlogConfig
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

loadFiles :: (MonadKorrvigs m) => BlogConfig -> m (Map BlogUrl BlogContent, Map Id (BlogUrl, Maybe Day))
loadFiles cfg = do
  topLevelsSQL <- rSelectOne $ fromName (baseSelectMtdt BlogFiles) $ sqlId $ cfg ^. blogCfgNote
  let topLevels =
        M.fromList $
          fmap (BlogTopLevel *** parseTopContent) $
            M.toList $
              fromMaybe M.empty $
                topLevelsSQL >>= fromJSONM
  filesSQL <- rSelect $ do
    entry <- selectTable entriesTable
    file <- baseSelectTextMtdt BlogFile $ entry ^. sqlEntryId
    pub <- selectTextMtdt PublishedDate $ entry ^. sqlEntryId
    pure ((file, entry ^. sqlEntryName), pub)
  let files = M.fromList $ (BlogFilePlain *** BlogFromFile) . fst <$> filesSQL
  let filesMapping = mkMapping BlogFilePlain filesSQL
  postsSQL <- rSelect $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryKind .== sqlKind Note
    post <- baseSelectTextMtdt BlogPost $ entry ^. sqlEntryId
    pub <- selectTextMtdt PublishedDate $ entry ^. sqlEntryId
    pure ((post, entry ^. sqlEntryName), pub)
  let posts = M.fromList $ (BlogPostNote *** BlogFromNote) . fst <$> postsSQL
  let postsMapping = mkMapping BlogPostNote postsSQL
  pure (topLevels <> files <> posts, filesMapping <> postsMapping)
  where
    parsePub :: Text -> Maybe Day
    parsePub = parseTimeM True defaultTimeLocale "%F" . T.unpack
    mkMapping :: (Text -> BlogUrl) -> [((Text, Id), Maybe Text)] -> Map Id (BlogUrl, Maybe Day)
    mkMapping f = M.fromList . fmap (view (_1 . _2) &&& (f . fst . fst &&& (>>= parsePub) . snd))
    parseTopContent :: Text -> BlogContent
    parseTopContent targetId = case T.split (== '#') targetId of
      ["", code] -> BlogFromCode (cfg ^. blogCfgNote) code
      [i, code] -> BlogFromCode (MkId i) code
      _ -> BlogFromNote $ MkId targetId

loadStructure :: (MonadKorrvigs m) => BlogConfig -> m BlogStructure
loadStructure cfg = do
  mtdt <- loadMtdt cfg
  (files, entries) <- loadFiles cfg
  pure $ BlogStructure mtdt files entries
