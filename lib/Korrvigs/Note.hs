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

import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.KindData
import Korrvigs.Note.AST
import Korrvigs.Note.Loc
import Korrvigs.Note.Pandoc
import Korrvigs.Note.Render

instance IsKD Note where
  data KDIdentifier Note = NoteIdentifier FilePath
    deriving (Ord, Eq)
  dSync = undefined
  dSyncOne = undefined

displayNoteId :: KDIdentifier Note -> Text
displayNoteId (NoteIdentifier path) = "note:" <> T.pack path
