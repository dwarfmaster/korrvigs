{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Event where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Event.Sync
import Korrvigs.KindData

instance IsKD Event where
  data KDIdentifier Event = EventIdentifier FilePath
    deriving (Ord, Eq)
  dSync _ = fmap (,M.empty) <$> dSyncImpl
  dSyncOne (EventIdentifier path) = (,M.empty) <$> dSyncOneImpl path

displayEventId :: KDIdentifier Event -> Text
displayEventId (EventIdentifier path) = "event:" <> T.pack path
