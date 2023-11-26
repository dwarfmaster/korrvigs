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

occupiesSpatialRegionAtRel ::
  Table
    (Field SqlInt8, Field SqlInt8, Field SqlInt8)
    (Field SqlInt8, Field SqlInt8, Field SqlInt8)
occupiesSpatialRegionAtRel =
  table
    "occupies_spatial_region_at"
    ( p3
        ( tableField "continuant",
          tableField "region",
          tableField "time"
        )
    )

occupiesSpatiotemporalRegionRel ::
  Table
    (Field SqlInt8, Field SqlInt8)
    (Field SqlInt8, Field SqlInt8)
occupiesSpatiotemporalRegionRel =
  table
    "occupies_spatiotemporal_region"
    ( p2
        ( tableField "occurrent",
          tableField "region"
        )
    )

occupiesTemporalRegionRel ::
  Table
    (Field SqlInt8, Field SqlInt8)
    (Field SqlInt8, Field SqlInt8)
occupiesTemporalRegionRel =
  table
    "occupies_temporal_region"
    ( p2
        ( tableField "continuant",
          tableField "region"
        )
    )

occurrentPartOfRel ::
  Table
    (Field SqlInt8, Field SqlInt8)
    (Field SqlInt8, Field SqlInt8)
occurrentPartOfRel =
  table
    "occurrent_part_of"
    ( p2
        ( tableField "part",
          tableField "whole"
        )
    )

spatiallyProjectsIntoRel ::
  Table
    (Field SqlInt8, Field SqlInt8)
    (Field SqlInt8, Field SqlInt8)
spatiallyProjectsIntoRel =
  table
    "spatially_projects_into"
    ( p2
        ( tableField "region",
          tableField "space"
        )
    )

temporallyProjectsIntoRel ::
  Table
    (Field SqlInt8, Field SqlInt8)
    (Field SqlInt8, Field SqlInt8)
temporallyProjectsIntoRel =
  table
    "temporally_projects_into"
    ( p2
        ( tableField "region",
          tableField "time"
        )
    )
