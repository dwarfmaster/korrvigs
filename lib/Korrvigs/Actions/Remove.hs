module Korrvigs.Actions.Remove where

import Control.Lens
import Korrvigs.Actions.SQL
import Korrvigs.AllEntries ()
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Compute (rmComputations)
import Korrvigs.Entry
import qualified Korrvigs.Event.Sync as Event
import qualified Korrvigs.File.Sync as File
import qualified Korrvigs.Link.Sync as Link
import Korrvigs.Monad
import qualified Korrvigs.Note.Sync as Note

remove :: (MonadKorrvigs m) => Entry -> m ()
remove entry = do
  removeDB entry
  rmComputations $ entry ^. name
  case entry ^. kindData of
    NoteD note -> Note.remove note
    LinkD link -> Link.remove link
    FileD file -> File.remove file
    EventD ev -> Event.remove ev
    CalendarD cal -> Cal.remove cal
