module Korrvigs.Note.New (new, NewNote (..), nnTitle, nnParent) where

import Control.Lens
import Data.Aeson
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Vector as V
import Korrvigs.Entry
import Korrvigs.KindData
import Korrvigs.Monad
import Korrvigs.Note.AST
import Korrvigs.Note.Render
import Korrvigs.Note.Sync
import Korrvigs.Utils.DateTree

data NewNote = NewNote
  { _nnTitle :: Text,
    _nnParent :: Maybe Id
  }

makeLenses ''NewNote

jsonSingleton :: (ToJSON a) => a -> Value
jsonSingleton v = Array $ V.singleton $ toJSON v

new :: (MonadKorrvigs m) => NewNote -> m Id
new note = do
  let idmk =
        imk "note"
          & idTitle ?~ note ^. nnTitle
          & idParent .~ note ^. nnParent
  i <- newId idmk
  let jsonParent = jsonSingleton . unId <$> note ^. nnParent
  let mtdt = M.fromList $ ("title", toJSON $ note ^. nnTitle) : maybeToList (("parents",) <$> jsonParent)
  let doc =
        Document
          { _docMtdt = mtdt,
            _docContent = [],
            _docTitle = note ^. nnTitle,
            _docRefTo = S.empty,
            _docChecks = (0, 0, 0),
            _docParents = maybe S.empty S.singleton $ note ^. nnParent
          }
  let bs = writeNoteLazy doc
  rt <- noteDirectory
  path <- storeFile rt noteTreeType Nothing (unId i <> ".md") bs
  relData <- dSyncOneImpl path
  atomicInsertRelData i relData
  pure i
