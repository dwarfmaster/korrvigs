{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Event where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Event.Sync
import Korrvigs.KindData

instance IsKD Event where
  data KDIdentifier Event = EventIdentifier FilePath
    deriving (Ord, Eq)
  dList _ = S.map EventIdentifier <$> dListImpl
  dGetId (EventIdentifier path) = view _1 $ eventIdFromPath path
  dSync _ = fmap (,M.empty) <$> dSyncImpl
  dSyncOne (EventIdentifier path) = (,M.empty) <$> dSyncOneImpl path

displayEventId :: KDIdentifier Event -> Text
displayEventId (EventIdentifier path) = "event:" <> T.pack path
