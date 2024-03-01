module Korrvigs.Kind.SQL where

import Data.Profunctor.Product.Default
import Korrvigs.Kind.Def (Kind (..))
import Opaleye
import Opaleye.Experimental.Enum

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

sqlKind :: Kind -> Field SqlKind
sqlKind = toFields
