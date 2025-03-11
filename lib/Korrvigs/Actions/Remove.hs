module Korrvigs.Actions.Remove where

import Control.Lens
import Korrvigs.Actions.SQL
import Korrvigs.AllEntries ()
import Korrvigs.Compute (rmComputations)
import Korrvigs.Entry
import Korrvigs.KindData
import Korrvigs.Monad

dispatchRemove :: (MonadKorrvigs m) => Entry -> m ()
dispatchRemove entry = do
  rmComputations $ entry ^. name
  case entry ^. kindData of
    NoteD note -> dRemove (dIdentify note)
    LinkD link -> dRemove (dIdentify link)
    FileD file -> dRemove (dIdentify file)
    EventD event -> dRemove (dIdentify event)
    CalendarD cal -> dRemove (dIdentify cal)

remove :: (MonadKorrvigs m) => Id -> m ()
remove i = do
  entry <- load i
  mapM_ (\e -> dispatchRemove e >> removeDB e) entry
