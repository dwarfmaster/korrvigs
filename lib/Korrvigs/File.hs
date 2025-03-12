{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.File (new, NewFile (..), nfEntry, displayFileId) where

import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.File.New
import Korrvigs.KindData

instance IsKD File where
  data KDIdentifier File = FileIdentifier FilePath
    deriving (Ord, Eq)
  dSync = undefined
  dSyncOne = undefined

displayFileId :: KDIdentifier File -> Text
displayFileId (FileIdentifier path) = "file:" <> T.pack path
