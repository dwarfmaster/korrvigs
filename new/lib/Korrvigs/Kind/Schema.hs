module Korrvigs.Kind.Schema where

import Data.Profunctor.Product (p6)
import Data.Profunctor.Product.Default
import Korrvigs.Geometry
import Korrvigs.Kind.Entry (Kind (..))
import Opaleye
import Opaleye.Experimental.Enum

-- Sql Kind
data SqlKind

fromSqlKind :: String -> Maybe Kind
fromSqlKind "note" = Just Note
fromSqlKind "link" = Just Link
fromSqlKind _ = Nothing

toSqlKind :: Kind -> String
toSqlKind Note = "note"
toSqlKind Link = "link"

sqlKindMapper :: EnumMapper SqlKind Kind
sqlKindMapper = enumMapper "kind" fromSqlKind toSqlKind

instance DefaultFromField SqlKind Kind where
  defaultFromField = enumFromField sqlKindMapper

instance Default ToFields Kind (Field SqlKind) where
  def = enumToFields sqlKindMapper

entriesTable ::
  Table
    (Field SqlText, Field SqlKind, FieldNullable SqlTimestamptz, FieldNullable SqlGeometry, FieldNullable SqlText, Field SqlJsonb)
    (Field SqlText, Field SqlKind, FieldNullable SqlTimestamptz, FieldNullable SqlGeometry, FieldNullable SqlText, Field SqlJsonb)
entriesTable =
  table
    "entries"
    (p6 (tableField "name", tableField "kind", tableField "date", tableField "geo", tableField "text", tableField "metadata"))
