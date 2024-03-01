module Korrvigs.Entry.SQL where

import Data.Profunctor.Product (p6)
import Korrvigs.Geometry
import Korrvigs.Kind.SQL
import Opaleye

-- Sql Kind
entriesTable ::
  Table
    (Field SqlText, Field SqlKind, FieldNullable SqlTimestamptz, FieldNullable SqlGeometry, FieldNullable SqlText, FieldNullable SqlJsonb)
    (Field SqlText, Field SqlKind, FieldNullable SqlTimestamptz, FieldNullable SqlGeometry, FieldNullable SqlText, FieldNullable SqlJsonb)
entriesTable =
  table
    "entries"
    (p6 (tableField "name", tableField "kind", tableField "date", tableField "geo", tableField "text", tableField "metadata"))
