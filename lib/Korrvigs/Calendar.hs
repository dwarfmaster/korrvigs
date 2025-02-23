{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Calendar where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Monad
import System.FilePath (joinPath)

instance IsKD Calendar where
  data KDIdentifier Calendar = CalIdentifier FilePath
    deriving (Ord, Eq)
  dLoad = undefined
  dRemoveDB _ = undefined
  dList _ = undefined
  dGetId _ = undefined
  dListCompute _ = undefined
  dSync _ = undefined
  dSyncOne _ = undefined
  dRemove _ = undefined
  dUpdateMetadata = undefined
  dKind = const Calendar
  dEntry = view calEntry
  dIdentify cal = CalIdentifier $ T.unpack $ cal ^. calEntry . name . to unId <> ".json"
  dToData = CalendarD

displayCalId :: KDIdentifier Calendar -> Text
displayCalId (CalIdentifier path) = "calendar:" <> T.pack path

calendarPath :: (MonadKorrvigs m) => Calendar -> m FilePath
calendarPath cal = do
  rt <- root
  let basename = T.unpack $ cal ^. calEntry . name . to unId <> ".json"
  pure $ joinPath [rt, "calendars", basename]
