module Korrvigs.Note.New (new, NewNote (..), nnEntry, nnTitle, nnTitleOverride) where

import Control.Arrow (first)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Foldable
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.Kind
import qualified Korrvigs.Link.New as Link
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Monad.Sync
import Korrvigs.Note.AST
import Korrvigs.Note.Render
import Korrvigs.Note.Sync
import Korrvigs.Utils.DateTree
import Network.URI (parseURI)
import Opaleye hiding (not, null)

data NewNote = NewNote
  { _nnEntry :: NewEntry,
    _nnTitle :: Text,
    _nnTitleOverride :: Bool
  }

makeLenses ''NewNote

new :: (MonadKorrvigs m) => NewNote -> m Id
new note = do
  mi <- rSelect $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryKind .== sqlKind Note
    t <- baseSelectTextMtdt Title $ entry ^. sqlEntryName
    where_ $ t .== sqlStrictText (note ^. nnTitle)
    pure $ entry ^. sqlEntryName
  case mi of
    (i : _) -> pure i
    [] -> create note

initContent :: Map (CI Text) Value -> [Block]
initContent mtdt =
  mconcat
    [ [Para [PlainLink Nothing uri] | url <- get Url, uri <- toList (parseURI $ T.unpack url)],
      [Embed (MkId i) | i <- get Cover],
      [Para (mkContent txt) | txt <- get Abstract]
    ]
  where
    get :: (ExtraMetadata mt, MtdtType mt ~ a, FromJSON a) => mt -> [a]
    get m = toList $ extractMtdt m mtdt
    mkContent :: Text -> [Inline]
    mkContent = intersperse Space . fmap Plain . filter (not . T.null) . T.split (== ' ')

create :: (MonadKorrvigs m) => NewNote -> m Id
create note = do
  idmk' <- applyNewEntry (note ^. nnEntry) (imk "note")
  extracted <- case note ^? nnEntry . neMtdt . at (mtdtName Url) . _Just . _String of
    Just url -> catchIOWith mempty $ Link.downloadInformation url
    Nothing -> pure mempty
  let title =
        if note ^. nnTitleOverride
          then fromMaybe (note ^. nnTitle) $ extracted ^? Link.exMtdt . at (mtdtSqlName Title) . _Just . _JSON
          else note ^. nnTitle
  let idmk = idmk' & idTitle ?~ title
  i <- newId idmk
  let parents = note ^. nnEntry . neParents
  let mtdt =
        useMtdt (note ^. nnEntry) $
          mconcat
            [ M.fromList $ first CI.mk <$> M.toList (extracted ^. Link.exMtdt),
              M.singleton (mtdtName Title) (toJSON title),
              maybe M.empty (M.singleton (mtdtName Language) . toJSON) (note ^. nnEntry . neLanguage),
              if null parents then M.empty else M.singleton (CI.mk "parents") (toJSON $ unId <$> parents),
              maybe M.empty (M.singleton (CI.mk "date") . toJSON) (note ^. nnEntry . neDate)
            ]
  let doc =
        Document
          { _docMtdt = mtdt,
            _docContent = initContent mtdt,
            _docTitle = title,
            _docRefTo = S.empty,
            _docChecks = def,
            _docParents = S.fromList $ note ^. nnEntry . neParents,
            _docTask = Nothing,
            _docTasks = [],
            _docCollections = S.empty,
            _docNamedSubs = S.empty,
            _docNamedCode = S.empty
          }
  let bs = writeNoteLazy doc
  rt <- noteDirectory
  path <- storeFile rt noteTreeType Nothing (unId i <> ".md") $ FileLazy bs
  syncFileOfKind path Note
  applyCollections (note ^. nnEntry) i
  applyChildren (note ^. nnEntry) i
  pure i
