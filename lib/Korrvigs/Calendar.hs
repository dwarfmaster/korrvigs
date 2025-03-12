{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Calendar (displayCalId, calendarPath) where

import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Calendar.Sync
import Korrvigs.Entry
import Korrvigs.KindData

instance IsKD Calendar where
  data KDIdentifier Calendar = CalIdentifier FilePath
    deriving (Ord, Eq)
  dSync = undefined
  dSyncOne = undefined

displayCalId :: KDIdentifier Calendar -> Text
displayCalId (CalIdentifier path) = "calendar:" <> T.pack path
