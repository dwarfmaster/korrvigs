module Korrvigs.Kind.SQL where

import Data.Profunctor.Product.Default
import Korrvigs.Kind.Def (Kind (..))
import Korrvigs.Utils.Opaleye (makeSqlMapper)
import Opaleye
import Opaleye.Experimental.Enum

data SqlKind

toSqlKind :: Kind -> String
toSqlKind Note = "note"
toSqlKind File = "file"
toSqlKind Event = "event"
toSqlKind Calendar = "calendar"
toSqlKind Syndicate = "syndicate"

sqlKindMapper :: EnumMapper SqlKind Kind
sqlKindMapper = makeSqlMapper "kind" toSqlKind

instance DefaultFromField SqlKind Kind where
  defaultFromField = enumFromField sqlKindMapper

instance Default ToFields Kind (Field SqlKind) where
  def = enumToFields sqlKindMapper

instance IsSqlType SqlKind where
  showSqlType _ = "kind"

sqlKind :: Kind -> Field SqlKind
sqlKind = toFields
