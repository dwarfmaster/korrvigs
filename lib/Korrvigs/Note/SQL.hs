{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Note.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import GHC.Int (Int64)
import Korrvigs.Actions.Utils
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Opaleye

data NoteRowImpl a b = NoteRow
  { _sqlNoteName :: a,
    _sqlNotePath :: b
  }

makeLenses ''NoteRowImpl
$(makeAdaptorAndInstanceInferrable "pNoteRow" ''NoteRowImpl)

type NoteRow = NoteRowImpl Id FilePath

mkNoteRow :: Id -> FilePath -> NoteRow
mkNoteRow = NoteRow

type NoteRowSQL = NoteRowImpl (Field SqlText) (Field SqlText)

instance Default ToFields NoteRow NoteRowSQL where
  def = pNoteRow $ NoteRow def def

notesTable :: Table NoteRowSQL NoteRowSQL
notesTable =
  table "notes" $
    pNoteRow $
      NoteRow
        (nameKindField Note)
        (tableField "path")

noteFromRow :: NoteRow -> Entry -> Note
noteFromRow nrow entry = MkNote entry (nrow ^. sqlNotePath)

sqlLoad :: (MonadKorrvigs m) => Id -> ((Entry -> Note) -> Entry) -> m (Maybe Entry)
sqlLoad = genSqlLoad notesTable (view sqlNoteName) noteFromRow

sqlRemove :: Id -> [Delete Int64]
sqlRemove = genSqlRemove notesTable $ view sqlNoteName
