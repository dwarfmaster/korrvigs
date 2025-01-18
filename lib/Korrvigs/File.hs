{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.File (new, NewFile (..), nfEntry, displayFileId) where

import Control.Lens
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.File.New
import Korrvigs.File.Sync
import Korrvigs.Kind
import Korrvigs.KindData

instance IsKD File where
  data KDIdentifier File = FileIdentifier FilePath
    deriving (Ord, Eq)
  dLoad = dLoadImpl
  dRemoveDB _ = dRemoveDBImpl
  dList _ = S.map FileIdentifier <$> dListImpl
  dGetId (FileIdentifier path) = dGetIdImpl path
  dSync _ = dSyncImpl
  dSyncOne (FileIdentifier path) = dSyncOneImpl path
  dRemove (FileIdentifier path) = dRemoveImpl path
  dUpdateMetadata = dUpdateMetadataImpl
  dKind = const File
  dEntry = view fileEntry
  dIdentify = FileIdentifier . view filePath
  dToData = FileD

displayFileId :: KDIdentifier File -> Text
displayFileId (FileIdentifier path) = "file:" <> T.pack path
