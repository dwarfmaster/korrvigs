module Korrvigs.Definition where

import Data.Text (Text)
import Data.UUID (UUID)
import Korrvigs.Classes (Class)

data Entry = MkEntry
  { entry_id :: UUID,
    entry_name :: Text,
    entry_notes :: FilePath
  }
  deriving (Show, Eq, Ord)

data Entity = MkEntity
  { entity_id :: Integer,
    entity_class :: Class,
    entity_uuid :: UUID,
    entity_sub :: Maybe Text,
    entity_query :: Maybe Text
  }
  deriving (Show, Eq, Ord)
