{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Entry.SQL where

import Control.Lens.TH (makeLenses)
import Data.Aeson (Value)
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time (CalendarDiffTime, ZonedTime)
import Korrvigs.Geometry
import Korrvigs.Kind
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

type EntryRow = EntryRowImpl Text Kind (Maybe ZonedTime) (Maybe CalendarDiffTime) (Maybe Geometry) (Maybe Text) (Maybe Value)

mkEntryRow :: Text -> Kind -> Maybe ZonedTime -> Maybe CalendarDiffTime -> Maybe Geometry -> Maybe Text -> Maybe Value -> EntryRow
mkEntryRow = EntryRow

type EntryRowSQL = EntryRowImpl (Field SqlText) (Field SqlKind) (FieldNullable SqlTimestamptz) (FieldNullable SqlInterval) (FieldNullable SqlGeometry) (FieldNullable SqlText) (FieldNullable SqlJsonb)

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
