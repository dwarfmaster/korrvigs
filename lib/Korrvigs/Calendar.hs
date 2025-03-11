{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Calendar (displayCalId, calendarPath) where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Calendar.Sync
import Korrvigs.Entry
import Korrvigs.KindData

instance IsKD Calendar where
  data KDIdentifier Calendar = CalIdentifier FilePath
    deriving (Ord, Eq)
  dList _ = S.map CalIdentifier <$> dListImpl
  dGetId (CalIdentifier path) = dGetIdImpl path
  dSync _ = dSyncImpl
  dSyncOne (CalIdentifier path) = dSyncOneImpl path

displayCalId :: KDIdentifier Calendar -> Text
displayCalId (CalIdentifier path) = "calendar:" <> T.pack path
