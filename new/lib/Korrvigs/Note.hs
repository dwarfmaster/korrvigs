{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Note
  ( Document (..),
    docMtdt,
    docContent,
    docTitle,
    docRefTo,
    Header (..),
    hdAttr,
    hdTitle,
    hdRefTo,
    hdLevel,
    hdContent,
    hdParent,
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
    displayNoteId,
  )
where

import Control.Lens (view)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Note.AST
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
  dSync _ = dSyncImpl
  dSyncOne (NoteIdentifier path) = dSyncOneImpl path
  dRemove (NoteIdentifier path) = dRemoveImpl path
  dKind = const Note
  dEntry = view noteEntry
  dIdentify = NoteIdentifier . view notePath
  dToData = NoteD

displayNoteId :: KDIdentifier Note -> Text
displayNoteId (NoteIdentifier path) = "note:" <> T.pack path
