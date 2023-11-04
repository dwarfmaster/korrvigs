module Korrvigs.Schema where

import Data.Profunctor.Product (p2, p3, p5)
import Opaleye

classesTable ::
  Table
    (Field SqlText, Field SqlText)
    (Field SqlText, Field SqlText)
classesTable =
  table
    "classes"
    ( p2
        ( tableField "class_name",
          tableField "parent_class"
        )
    )

entriesTable ::
  Table
    (Field SqlUuid, Field SqlText, Field SqlText)
    (Field SqlUuid, Field SqlText, Field SqlText)
entriesTable =
  table
    "entries"
    ( p3
        ( tableField "entry_id",
          tableField "entry_name",
          tableField "entry_notes"
        )
    )

entitiesTable ::
  Table
    ((), Field SqlText, Field SqlUuid, FieldNullable SqlText, FieldNullable SqlText)
    (Field SqlInt8, Field SqlText, Field SqlUuid, FieldNullable SqlText, FieldNullable SqlText)
entitiesTable =
  table
    "entities"
    ( p5
        ( readOnlyTableField "entity_id",
          tableField "entity_class",
          tableField "entity_uuid",
          tableField "entity_sub",
          tableField "entity_query"
        )
    )

classEntryTable ::
  Table
    (Field SqlText, Field SqlUuid)
    (Field SqlText, Field SqlUuid)
classEntryTable =
  table
    "class_entry"
    ( p2
        ( tableField "class_entry_class",
          tableField "class_entry_entry"
        )
    )
