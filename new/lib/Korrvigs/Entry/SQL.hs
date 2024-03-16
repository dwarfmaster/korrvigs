{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Entry.SQL where

import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time (CalendarDiffTime, ZonedTime)
import Korrvigs.FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Monad
import Opaleye

-- Entries table
data EntryRowImpl a b c d e f g = EntryRow
  { _sqlEntryName :: a,
    _sqlEntryKind :: b,
    _sqlEntryDate :: c,
    _sqlEntryDuration :: d,
    _sqlEntryGeo :: e,
    _sqlEntryText :: f,
    _sqlEntryMetadata :: g
  }

makeLenses ''EntryRowImpl
$(makeAdaptorAndInstanceInferrable "pEntryRow" ''EntryRowImpl)

type EntryRow = EntryRowImpl Text Kind (Maybe ZonedTime) (Maybe CalendarDiffTime) (Maybe Geometry) (Maybe ()) (Maybe Value)

mkEntryRow :: Text -> Kind -> Maybe ZonedTime -> Maybe CalendarDiffTime -> Maybe Geometry -> Maybe () -> Maybe Value -> EntryRow
mkEntryRow = EntryRow

type EntryRowSQL = EntryRowImpl (Field SqlText) (Field SqlKind) (FieldNullable SqlTimestamptz) (FieldNullable SqlInterval) (FieldNullable SqlGeometry) (FieldNullable SqlTSVector) (FieldNullable SqlJsonb)

instance Default ToFields EntryRow EntryRowSQL where
  def = pEntryRow $ EntryRow def def def def def def def

entriesTable ::
  Table EntryRowSQL EntryRowSQL
entriesTable =
  table "entries" $
    pEntryRow $
      EntryRow
        (tableField "name")
        (tableField "kind")
        (tableField "date")
        (tableField "duration")
        (tableField "geo")
        (tableField "text")
        (tableField "metadata")

-- Returns True if the insertion was successful
addEntry :: MonadKorrvigs m => EntryRow -> m Bool
addEntry entry =
  pgSQL >>= \conn -> liftIO $ do
    cnt <-
      runInsert conn $
        Insert
          { iTable = entriesTable,
            iRows = [toFields entry],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    pure $ cnt == 1
