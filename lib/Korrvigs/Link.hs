{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Link
  ( displayLinkId,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.KindData
import Prelude hiding (readFile)

instance IsKD Link where
  data KDIdentifier Link = LinkIdentifier FilePath
    deriving (Ord, Eq)
  dSync = undefined
  dSyncOne = undefined

displayLinkId :: KDIdentifier Link -> Text
displayLinkId (LinkIdentifier path) = "link:" <> T.pack path
