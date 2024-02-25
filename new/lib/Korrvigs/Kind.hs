{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Korrvigs.Kind (Kind (..), KindData (..), dataKind, SqlKind) where

import Data.Profunctor.Product (p3)
import Data.Profunctor.Product.Default (Default, def)
import Data.Text (Text)
import Opaleye
import Opaleye.Experimental.Enum (EnumMapper, enumFromField, enumMapper, enumToFields)

data Kind
  = Note
  | Link
  deriving (Eq, Ord, Show, Enum)

data KindData
  = NoteData {note_file :: Text}
  | LinkData {link_href :: Text}
  deriving (Eq, Ord, Show)

dataKind :: KindData -> Kind
dataKind (NoteData _) = Note
dataKind (LinkData _) = Link

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

noteTable ::
  Table
    (Field SqlText, Field SqlKind, Field SqlText)
    (Field SqlText, Field SqlKind, Field SqlText)
noteTable = table "notes" (p3 (tableField "name", tableField "kind", tableField "path"))
