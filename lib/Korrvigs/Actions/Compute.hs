module Korrvigs.Actions.Compute where

import Control.Lens
import Korrvigs.Calendar ()
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Event ()
import Korrvigs.File ()
import Korrvigs.KindData
import Korrvigs.Link ()
import Korrvigs.Monad
import Korrvigs.Note ()

listCompute :: (MonadKorrvigs m) => Entry -> m EntryComps
listCompute entry = case entry ^. kindData of
  LinkD link -> dListCompute $ dIdentify link
  FileD file -> dListCompute $ dIdentify file
  NoteD note -> dListCompute $ dIdentify note
  EventD event -> dListCompute $ dIdentify event
  CalendarD cal -> dListCompute $ dIdentify cal
