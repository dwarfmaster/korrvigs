{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Event where

import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.KindData

instance IsKD Event where
  data KDIdentifier Event = EventIdentifier FilePath
    deriving (Ord, Eq)
  dSync = undefined
  dSyncOne = undefined

displayEventId :: KDIdentifier Event -> Text
displayEventId (EventIdentifier path) = "event:" <> T.pack path
