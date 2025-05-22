{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Note.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import GHC.Int (Int64)
import Korrvigs.Actions.Utils
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Opaleye

data NoteRowImpl a b c = NoteRow
  { _sqlNoteName :: a,
    _sqlNotePath :: b,
    _sqlNoteCollections :: c
  }

makeLenses ''NoteRowImpl
$(makeAdaptorAndInstanceInferrable "pNoteRow" ''NoteRowImpl)

type NoteRow = NoteRowImpl Id FilePath [Text]

mkNoteRow :: Id -> FilePath -> [Text] -> NoteRow
mkNoteRow = NoteRow

type NoteRowSQL = NoteRowImpl (Field SqlText) (Field SqlText) (Field (SqlArray SqlText))

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

noteFromRow :: NoteRow -> Entry -> Note
noteFromRow nrow entry = MkNote entry (nrow ^. sqlNotePath)

sqlLoad :: (MonadKorrvigs m) => Id -> ((Entry -> Note) -> Entry) -> m (Maybe Entry)
sqlLoad = genSqlLoad notesTable (view sqlNoteName) noteFromRow

sqlRemove :: Id -> [Delete Int64]
sqlRemove = genSqlRemove notesTable $ view sqlNoteName
