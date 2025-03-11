{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Calendar (displayCalId, calendarPath) where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Calendar.Sync
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData

instance IsKD Calendar where
  data KDIdentifier Calendar = CalIdentifier FilePath
    deriving (Ord, Eq)
  dLoad = undefined
  dRemoveDB _ = undefined
  dList _ = S.map CalIdentifier <$> dListImpl
  dGetId (CalIdentifier path) = dGetIdImpl path
  dListCompute _ = pure M.empty
  dSync _ = dSyncImpl
  dSyncOne (CalIdentifier path) = dSyncOneImpl path
  dRemove (CalIdentifier path) = dRemoveImpl path
  dUpdateMetadata = dUpdateMetadataImpl
  dUpdateParents = dUpdateParentsImpl
  dKind = const Calendar
  dEntry = view calEntry
  dIdentify = CalIdentifier . calBasename . view (calEntry . name)
  dToData = CalendarD

displayCalId :: KDIdentifier Calendar -> Text
displayCalId (CalIdentifier path) = "calendar:" <> T.pack path
