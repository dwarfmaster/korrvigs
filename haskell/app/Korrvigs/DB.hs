module Korrvigs.DB where

import Korrvigs.Schema
import Opaleye

-- Given the UUID of an entry, create a query giving the id of its root
rootFor :: Field SqlUuid -> Select (Field SqlInt8, Field SqlText)
rootFor uuid = do
  (id_, cls_, entry_, sub_, query_) <- selectTable entitiesTable
  where_ $ entry_ .== uuid
  where_ $ isNull sub_ .&& isNull query_
  pure (id_, cls_)
