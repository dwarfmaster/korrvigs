module Korrvigs.AllEntries where

import Korrvigs.Entry
import Korrvigs.KindData
import Korrvigs.Link ()

newtype AnyIdentifier
  = LinkID (KDIdentifier Link)
