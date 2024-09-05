module Korrvigs.Utils.Base16 where

import Control.Lens hiding (Cons)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Aeson.Key as K
import Data.Maybe
import Data.Text (Text)
import Data.Yaml
import Prelude hiding (head, tail)

data Base16Index
  = Base00
  | Base01
  | Base02
  | Base03
  | Base04
  | Base05
  | Base06
  | Base07
  | Base08
  | Base09
  | Base0A
  | Base0B
  | Base0C
  | Base0D
  | Base0E
  | Base0F
  deriving (Eq, Enum, Bounded)

baseName :: Base16Index -> Text
baseName Base00 = "base00"
baseName Base01 = "base01"
baseName Base02 = "base02"
baseName Base03 = "base03"
baseName Base04 = "base04"
baseName Base05 = "base05"
baseName Base06 = "base06"
baseName Base07 = "base07"
baseName Base08 = "base08"
baseName Base09 = "base09"
baseName Base0A = "base0A"
baseName Base0B = "base0B"
baseName Base0C = "base0C"
baseName Base0D = "base0D"
baseName Base0E = "base0E"
baseName Base0F = "base0F"

assemble :: (Applicative p) => (Base16Index -> p a) -> p (Base16Index -> a)
assemble f =
  llookup <$> traverse (\i -> (i,) <$> f i) [minBound .. maxBound]
  where
    llookup values = fromJust . flip lookup values

newtype Base16Data = B16Data {_extractB16Data :: Base16Index -> Text}

makeLenses ''Base16Data

instance FromJSON Base16Data where
  parseJSON = withObject "Base16Data" $ \v ->
    B16Data <$> assemble (\b -> v .: K.fromText (baseName b))

readB16FromYaml :: (MonadIO m) => FilePath -> m Base16Data
readB16FromYaml = liftIO . decodeFileThrow

theme16 :: Base16Data -> Base16Index -> Text
theme16 (B16Data l) i = l i
