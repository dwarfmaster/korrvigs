{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Link where

import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Prelude hiding (readFile)

instance IsKD Link where
  data KDMaker Link = LinkMaker
  data KDUpdater Link = LinkUpdater
  data KDIdentifier Link = LinkIdentifier
  dLoad = undefined
  dMake = undefined
  dUpdate = undefined
  dList = undefined
  dGetId = undefined
  dSync = undefined
  dSyncOne = undefined
  dRemove = undefined
  dKind = const Link
  dEntry = _linkEntry
  dIdentify = undefined
  dToData = LinkD
