module Korrvigs.Compute.Declare where

import Control.Lens hiding ((.=))
import Crypto.Hash
import Data.Aeson
import Data.Default
import Data.Text
import qualified Data.Text as T
import Korrvigs.Entry

type CompHash = Digest SHA256

data CompType
  = ScalarImage
  | Picture
  | VectorImage
  | Json
  deriving (Eq, Show)

instance FromJSON CompType where
  parseJSON = withText "CompType" $ \case
    "scalar" -> pure ScalarImage
    "picture" -> pure Picture
    "vector" -> pure VectorImage
    "json" -> pure Json
    str -> fail $ T.unpack $ "\"" <> str <> "\" is not a valid computation type name"

instance ToJSON CompType where
  toJSON ScalarImage = "scalar"
  toJSON Picture = "picture"
  toJSON VectorImage = "vector"
  toJSON Json = "json"

data Dependencies = Deps
  { _depEntries :: [Id],
    _depIO :: Bool,
    _depComps :: [(Id, Text)]
  }
  deriving (Show, Eq, Ord)

makeLenses ''Dependencies

instance Default Dependencies where
  def = Deps [] False []

data ActionData = ActionData
  { _adatType :: CompType,
    _adatDeps :: Dependencies,
    _adatContentAddressed :: Maybe CompHash
  }
  deriving (Show, Eq)

makeLenses ''ActionData
