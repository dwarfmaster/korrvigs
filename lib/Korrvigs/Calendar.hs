module Korrvigs.Calendar (calendarPath, listCalendars) where

import Control.Lens
import Data.Maybe
import Korrvigs.Calendar.SQL
import Korrvigs.Calendar.Sync
import Korrvigs.Entry
import Korrvigs.Monad
import Opaleye

listCalendars :: (MonadKorrvigs m) => m [Calendar]
listCalendars = do
  calIds <- rSelect $ view sqlCalId <$> selectTable calendarsTable
  calEntries <- mapM loadSql calIds
  let toCal entry = case entry ^. entryKindData of
        CalendarD cal -> Just cal
        _ -> Nothing
  pure $ mapMaybe (>>= toCal) calEntries
