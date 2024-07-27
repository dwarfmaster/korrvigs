module Korrvigs.AllEntries where

import Korrvigs.Entry
import Korrvigs.KindData
import Korrvigs.Link ()
import Korrvigs.Note ()

data AnyIdentifier
  = LinkID (KDIdentifier Link)
  | NoteID (KDIdentifier Note)
