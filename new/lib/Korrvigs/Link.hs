{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Link where

import Korrvigs.Entry
import Korrvigs.Kind
import Prelude hiding (readFile)

instance IsKD Link where
  data KDMaker Link = LinkMaker
  data KDUpdater Link = LinkUpdater
  dLoad = undefined
  dMake = undefined
  dUpdate = undefined
  dList = undefined
  dSync = undefined
  dSyncOne = undefined
  dKind = const Link
  dEntry = _linkEntry
