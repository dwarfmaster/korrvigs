module Data.VCard.Defs where

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import Data.Time

data VCardAbstractValue = VCAbstractValue
  { _vcValTags :: Map Text Text,
    _vcValValue :: [Text]
  }
  deriving (Eq, Show)

data VCardFile = VCFile
  { _vcVersion :: Text,
    _vcUID :: Text,
    _vcContent :: Map Text [VCardAbstractValue],
    _vcAnniversary :: Maybe Day,
    _vcBDay :: Maybe Day,
    _vcEmail :: [Text],
    _vcFullName :: Maybe Text,
    _vcNicknames :: [Text],
    _vcTel :: Map Text [Text],
    _vcUrl :: Maybe Text
  }
  deriving (Eq, Show)

makeLenses ''VCardAbstractValue
makeLenses ''VCardFile
