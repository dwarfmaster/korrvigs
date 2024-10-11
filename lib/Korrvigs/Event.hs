{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Event where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Monad
import System.FilePath

instance IsKD Event where
  data KDIdentifier Event = EventIdentifier Text Text
    deriving (Ord, Eq)
  dLoad = undefined
  dRemoveDB = undefined
  dList _ = undefined
  dGetId (EventIdentifier _ _) = undefined
  dSync _ = undefined
  dSyncOne (EventIdentifier _ _) = undefined
  dRemove (EventIdentifier _ _) = undefined
  dKind = const Event
  dEntry = view eventEntry
  dIdentify ev = EventIdentifier (ev ^. eventCalendar) (ev ^. eventFile)
  dToData = EventD

displayEventId :: KDIdentifier Event -> Text
displayEventId (EventIdentifier cal file) = "event:" <> cal <> ":" <> file

calendarDirectory :: (MonadKorrvigs m) => Text -> m FilePath
calendarDirectory cal = joinPath . (: ["files", T.unpack cal]) <$> root

eventPath :: (MonadKorrvigs m) => Event -> m FilePath
eventPath ev =
  joinPath . (: [T.unpack (ev ^. eventFile)]) <$> calendarDirectory (ev ^. eventCalendar)
