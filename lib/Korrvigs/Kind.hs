module Korrvigs.Kind (Kind (..), SqlKind, sqlKind, displayKind) where

import Data.Text (Text)
import Korrvigs.Kind.Def
import Korrvigs.Kind.SQL

displayKind :: Kind -> Text
displayKind Link = "link"
displayKind Note = "note"
displayKind File = "file"
displayKind Event = "event"
