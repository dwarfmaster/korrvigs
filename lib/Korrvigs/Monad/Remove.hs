module Korrvigs.Monad.Remove where

import Control.Lens
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Entry
import qualified Korrvigs.Event.Sync as Event
import qualified Korrvigs.File.Sync as File
import qualified Korrvigs.Link.Sync as Link
import Korrvigs.Monad.Class
import Korrvigs.Monad.SQL
import qualified Korrvigs.Note.Sync as Note

remove :: (MonadKorrvigs m) => Entry -> m ()
remove entry = do
  removeDB entry
  case entry ^. entryKindData of
    NoteD note -> Note.remove note
    LinkD link -> Link.remove link
    FileD file -> File.remove file
    EventD ev -> Event.remove ev
    CalendarD cal -> Cal.remove cal
