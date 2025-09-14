{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Note.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
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
