{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Note.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad.Class
import Korrvigs.Monad.Utils
import Opaleye

-- notes table
data NoteRowImpl a b c = NoteRow
  { _sqlNoteId :: a,
    _sqlNotePath :: b,
    _sqlNoteCollections :: c
  }

makeLenses ''NoteRowImpl
$(makeAdaptorAndInstanceInferrable "pNoteRow" ''NoteRowImpl)

type NoteRow = NoteRowImpl Int FilePath [Text]

mkNoteRow :: Int -> FilePath -> [Text] -> NoteRow
mkNoteRow = NoteRow

type NoteRowSQL = NoteRowImpl (Field SqlInt4) (Field SqlText) (Field (SqlArray SqlText))

instance Default ToFields NoteRow NoteRowSQL where
  def = pNoteRow $ NoteRow def def def

notesTable :: Table NoteRowSQL NoteRowSQL
notesTable =
  table "notes" $
    pNoteRow $
      NoteRow
        (nameKindField Note)
        (tableField "path")
        (tableField "collections")

-- notes_collections table
data NoteColRowImpl a b c = NoteColRow
  { _sqlNoteColId :: a,
    _sqlNoteColName :: b,
    _sqlNoteColEntry :: c
  }

makeLenses ''NoteColRowImpl
$(makeAdaptorAndInstanceInferrable "pNoteColRow" ''NoteColRowImpl)

type NoteColRow = NoteColRowImpl Int Text Id

type NoteColRowSQL = NoteColRowImpl (Field SqlInt4) (Field SqlText) (Field SqlText)

instance Default ToFields NoteColRow NoteColRowSQL where
  def = pNoteColRow $ NoteColRow def def def

notesCollectionsTable :: Table NoteColRowSQL NoteColRowSQL
notesCollectionsTable =
  table "notes_collections" $
    pNoteColRow $
      NoteColRow
        (tableField "id")
        (tableField "name")
        (tableField "entry")

-- Functions
noteFromRow :: NoteRow -> Entry -> Note
noteFromRow nrow entry = MkNote entry (nrow ^. sqlNotePath)

sqlLoad :: (MonadKorrvigs m) => Int -> ((Entry -> Note) -> Entry) -> m (Maybe Entry)
sqlLoad = genSqlLoad notesTable (view sqlNoteId) noteFromRow

sqlRemove :: Int -> [Delete Int64]
sqlRemove i =
  genSqlRemove notesCollectionsTable (view sqlNoteColId) i
    ++ genSqlRemove notesTable (view sqlNoteId) i

-- notes_tasks table
data NoteTaskRowImpl a b c d e f g h = NoteTaskRow
  { _noteTaskNote :: a,
    _noteTaskTitle :: b,
    _noteTaskRef :: c,
    _noteTaskStatus :: d,
    _noteTaskScheduled :: e,
    _noteTaskDeadline :: f,
    _noteTaskStarted :: g,
    _noteTaskFinished :: h
  }

makeLenses ''NoteTaskRowImpl
$(makeAdaptorAndInstanceInferrable "pNoteTaskRow" ''NoteTaskRowImpl)

type NoteTaskRow =
  NoteTaskRowImpl Int Text Text Text (Maybe ZonedTime) (Maybe ZonedTime) (Maybe ZonedTime) (Maybe ZonedTime)

type NoteTaskRowSQL =
  NoteTaskRowImpl (Field SqlInt4) (Field SqlText) (Field SqlText) (Field SqlText) (FieldNullable SqlTimestamptz) (FieldNullable SqlTimestamptz) (FieldNullable SqlTimestamptz) (FieldNullable SqlTimestamptz)

type NoteTaskRowGen =
  NoteTaskRowImpl Int Text (Maybe Text) Text (Maybe ZonedTime) (Maybe ZonedTime) (Maybe ZonedTime) (Maybe ZonedTime)

type NoteTaskRowGenSQL =
  NoteTaskRowImpl (Field SqlInt4) (Field SqlText) (FieldNullable SqlText) (Field SqlText) (FieldNullable SqlTimestamptz) (FieldNullable SqlTimestamptz) (FieldNullable SqlTimestamptz) (FieldNullable SqlTimestamptz)

instance Default ToFields NoteTaskRow NoteTaskRowSQL where
  def = pNoteTaskRow $ NoteTaskRow def def def def def def def def

notesTasksTable :: Table NoteTaskRowSQL NoteTaskRowSQL
notesTasksTable =
  table "notes_tasks" $
    pNoteTaskRow $
      NoteTaskRow
        (tableField "note")
        (tableField "title")
        (tableField "hdref")
        (tableField "status")
        (tableField "scheduled")
        (tableField "deadline")
        (tableField "started")
        (tableField "finished")

makeGenTaskRow :: NoteTaskRowSQL -> NoteTaskRowGenSQL
makeGenTaskRow = noteTaskRef %~ toNullable
