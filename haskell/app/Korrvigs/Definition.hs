module Korrvigs.Definition where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import Korrvigs.Classes (Class)
import System.FilePath ((</>))

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
    entry_notes :: FilePath,
    entry_root :: Entity
  }
  deriving (Show, Eq, Ord)

entryMdPath :: FilePath -> Entry -> FilePath
entryMdPath root entry = root </> U.toString (entry_id entry) </> entry_notes entry

entrySubPath :: FilePath -> Entry -> Text -> FilePath
entrySubPath root = subPath root . entry_id

subPath :: FilePath -> UUID -> Text -> FilePath
subPath root entry sub = root </> U.toString entry </> T.unpack sub

data Entity = MkEntity
  { entity_id :: Int64,
    entity_class :: Class,
    entity_uuid :: UUID,
    entity_sub :: Maybe Text,
    entity_query :: Maybe Text
  }
  deriving (Show, Eq, Ord)
