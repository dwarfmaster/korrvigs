module Korrvigs.Relations.Generated where

import Data.Profunctor.Product
import Opaleye

continuantPartOfAtRel ::
  Table
    (Field SqlInt8, Field SqlInt8, Field SqlInt8)
    (Field SqlInt8, Field SqlInt8, Field SqlInt8)
continuantPartOfAtRel =
  table
    "continuant_part_of_at"
    ( p3
        ( tableField "part",
          tableField "whole",
          tableField "time_region"
        )
    )

denotesAtRel ::
  Table
    (Field SqlInt8, Field SqlInt8, Field SqlInt8)
    (Field SqlInt8, Field SqlInt8, Field SqlInt8)
denotesAtRel =
  table
    "denotes_at"
    ( p3
        ( tableField "identifier",
          tableField "entity",
          tableField "time_region"
        )
    )

identifierRel ::
  Table
    (Field SqlInt8, Field SqlText)
    (Field SqlInt8, Field SqlText)
identifierRel =
  table
    "identifier"
    ( p2
        ( tableField "identifier",
          tableField "val"
        )
    )
