module Korrvigs.Note.New (new, NewNote (..), nnEntry, nnTitle) where

import Control.Arrow (first)
import Control.Lens
import Data.Aeson
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Actions
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Note.AST
import Korrvigs.Note.Render
import Korrvigs.Note.Sync
import Korrvigs.Utils.DateTree
import Network.URI (parseURI)
import Opaleye hiding (not, null)

data NewNote = NewNote
  { _nnEntry :: NewEntry,
    _nnTitle :: Text
  }

makeLenses ''NewNote

new :: (MonadKorrvigs m) => NewNote -> m Id
new note = do
  mi <- rSelect $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryKind .== sqlKind Note
    t <- baseSelectTextMtdt Title $ entry ^. sqlEntryName
    where_ $ matchNullable (sqlBool False) (.== sqlStrictText (note ^. nnTitle)) t
    pure $ entry ^. sqlEntryName
  case mi of
    (i : _) -> pure i
    [] -> create note

initContent :: Map (CI Text) Value -> [Block]
initContent mtdt =
  mconcat
    [ [Para [PlainLink Nothing uri] | url <- get Url, uri <- toList (parseURI $ T.unpack url)],
      [Embed (MkId i) | i <- get Cover],
      [Para [Plain txt] | txt <- get Abstract]
    ]
  where
    get :: (ExtraMetadata mt, MtdtType mt ~ a, FromJSON a) => mt -> [a]
    get m = toList $ extractMtdt m mtdt

create :: (MonadKorrvigs m) => NewNote -> m Id
create note = do
  idmk' <- applyNewEntry (note ^. nnEntry) (imk "note")
  let idmk = idmk' & idTitle ?~ note ^. nnTitle
  i <- newId idmk
  let parents = note ^. nnEntry . neParents
  let mtdt =
        mconcat
          [ M.fromList (first CI.mk <$> note ^. nnEntry . neMtdt),
            M.singleton (mtdtName Title) (toJSON $ note ^. nnTitle),
            maybe M.empty (M.singleton (mtdtName Language) . toJSON) (note ^. nnEntry . neLanguage),
            if null parents then M.empty else M.singleton (CI.mk "parents") (toJSON $ unId <$> parents),
            maybe M.empty (M.singleton (CI.mk "date") . toJSON) (note ^. nnEntry . neDate)
          ]
  let doc =
        Document
          { _docMtdt = mtdt,
            _docContent = initContent mtdt,
            _docTitle = note ^. nnTitle,
            _docRefTo = S.empty,
            _docChecks = Checks 0 0 0 0 0,
            _docParents = S.fromList $ note ^. nnEntry . neParents,
            _docTask = Nothing,
            _docTasks = []
          }
  let bs = writeNoteLazy doc
  rt <- noteDirectory
  path <- storeFile rt noteTreeType Nothing (unId i <> ".md") $ FileLazy bs
  syncFileOfKind path Note
  pure i
