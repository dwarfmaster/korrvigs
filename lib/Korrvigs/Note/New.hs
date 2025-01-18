module Korrvigs.Note.New (new, NewNote (..), nnEntry, nnTitle) where

import Control.Lens
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.KindData
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
  let mtdt = M.fromList (note ^. nnEntry . neMtdt) `M.union` M.singleton "title" (toJSON $ note ^. nnTitle)
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
