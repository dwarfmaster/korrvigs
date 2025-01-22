{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Event where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Event.Sync
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Monad
import System.FilePath

instance IsKD Event where
  data KDIdentifier Event = EventIdentifier Id Text Text
    deriving (Ord, Eq)
  dLoad = dLoadImpl
  dRemoveDB _ = dRemoveDBImpl
  dList _ = S.map (\(i, cal, ics) -> EventIdentifier i cal ics) <$> dListImpl
  dGetId (EventIdentifier i _ _) = i
  dListCompute _ = pure M.empty
  dSync _ = fmap (,M.empty) <$> dSyncImpl
  dSyncOne (EventIdentifier _ cal ics) = (,M.empty) <$> dSyncOneImpl cal ics
  dRemove (EventIdentifier _ cal ics) = dRemoveImpl cal ics
  dUpdateMetadata = dUpdateMetadataImpl
  dKind = const Event
  dEntry = view eventEntry
  dIdentify ev = EventIdentifier (ev ^. eventEntry . name) (ev ^. eventCalendar) (ev ^. eventFile)
  dToData = EventD

displayEventId :: KDIdentifier Event -> Text
displayEventId (EventIdentifier _ cal file) = "event:" <> cal <> ":" <> file

calendarDirectory :: (MonadKorrvigs m) => Text -> m FilePath
calendarDirectory cal = joinPath . (: ["events", T.unpack cal]) <$> root

eventPath :: (MonadKorrvigs m) => Event -> m FilePath
eventPath ev =
  joinPath . (: [T.unpack (ev ^. eventFile)]) <$> calendarDirectory (ev ^. eventCalendar)
