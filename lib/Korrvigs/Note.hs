{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Note
  ( Document (..),
    docMtdt,
    docContent,
    docTitle,
    docRefTo,
    docChecks,
    Checks (..),
    ckTodo,
    ckOngoing,
    ckBlocked,
    ckDone,
    ckDont,
    Header (..),
    hdAttr,
    hdTitle,
    hdRefTo,
    hdChecks,
    hdLevel,
    hdContent,
    hdParent,
    hdTask,
    hdTasks,
    hdDocument,
    Block (..),
    Inline (..),
    Style (..),
    Attr (..),
    attrId,
    attrClasses,
    attrMtdt,
    Cell (..),
    Table (..),
    cellOrig,
    cellWidth,
    cellHeight,
    cellData,
    tableCaption,
    tableCells,
    tableAttr,
    tableHeader,
    tableFooter,
    readNote,
    writeNote,
    writeNoteLazy,
    writeHeaderLazy,
    displayNoteId,
    module Korrvigs.Note.Loc,
  )
where

import Control.Lens (view)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Note.AST
import Korrvigs.Note.Loc
import Korrvigs.Note.Pandoc
import Korrvigs.Note.Render
import Korrvigs.Note.Sync

instance IsKD Note where
  data KDIdentifier Note = NoteIdentifier FilePath
    deriving (Ord, Eq)
  dLoad = dLoadImpl
  dRemoveDB _ = dRemoveDBImpl
  dList _ = S.map NoteIdentifier <$> dListImpl
  dGetId (NoteIdentifier path) = dGetIdImpl path
  dListCompute _ = pure M.empty
  dSync _ = fmap (,M.empty) <$> dSyncImpl
  dSyncOne (NoteIdentifier path) = (,M.empty) <$> dSyncOneImpl path
  dRemove (NoteIdentifier path) = dRemoveImpl path
  dUpdateMetadata = dUpdateMetadataImpl
  dUpdateParents = dUpdateParentsImpl
  dKind = const Note
  dEntry = view noteEntry
  dIdentify = NoteIdentifier . view notePath
  dToData = NoteD

displayNoteId :: KDIdentifier Note -> Text
displayNoteId (NoteIdentifier path) = "note:" <> T.pack path
