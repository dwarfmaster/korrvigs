module Korrvigs.Definition where

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import Korrvigs.Classes (Class)

data EntityRef = EntityRef
  { ref_uuid :: UUID,
    ref_sub :: Maybe Text,
    ref_query :: Maybe Text
  }
  deriving (Eq, Ord)

instance Show EntityRef where
  show (EntityRef uuid sub query) = U.toString uuid ++ ssub ++ squery
    where
      optShow _ Nothing = ""
      optShow prefix (Just v) = prefix ++ T.unpack v
      ssub = optShow "/" sub
      squery = optShow "#" query

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
