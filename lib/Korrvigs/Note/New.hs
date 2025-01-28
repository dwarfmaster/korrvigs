module Korrvigs.Note.New (new, NewNote (..), nnEntry, nnTitle) where

import Control.Arrow (first)
import Control.Lens
import Data.Aeson
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.KindData
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Note.AST
import Korrvigs.Note.Render
import Korrvigs.Note.Sync
import Korrvigs.Utils.DateTree

data NewNote = NewNote
  { _nnEntry :: NewEntry,
    _nnTitle :: Text
  }

makeLenses ''NewNote

new :: (MonadKorrvigs m) => NewNote -> m Id
new note = do
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
            _docContent = [],
            _docTitle = note ^. nnTitle,
            _docRefTo = S.empty,
            _docChecks = (0, 0, 0),
            _docParents = S.fromList $ note ^. nnEntry . neParents
          }
  let bs = writeNoteLazy doc
  rt <- noteDirectory
  path <- storeFile rt noteTreeType Nothing (unId i <> ".md") bs
  relData <- dSyncOneImpl path
  atomicInsertRelData i relData
  pure i
