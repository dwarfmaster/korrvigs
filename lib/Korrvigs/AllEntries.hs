module Korrvigs.AllEntries where

import Korrvigs.Calendar ()
import Korrvigs.Entry
import Korrvigs.Event ()
import Korrvigs.File ()
import Korrvigs.KindData
import Korrvigs.Link ()
import Korrvigs.Note ()

data AnyIdentifier
  = LinkID (KDIdentifier Link)
  | NoteID (KDIdentifier Note)
  | FileID (KDIdentifier File)
  | EventID (KDIdentifier Event)
  | CalendarID (KDIdentifier Calendar)
