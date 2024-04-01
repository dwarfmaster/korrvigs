{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Link where

import Control.Lens (view)
import qualified Data.Set as S
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Link.SQL
import Korrvigs.Link.Sync
import Prelude hiding (readFile)

instance IsKD Link where
  data KDMaker Link = LinkMaker
    deriving (Ord, Eq)
  data KDUpdater Link = LinkUpdater
    deriving (Ord, Eq)
  data KDIdentifier Link = LinkIdentifier FilePath
    deriving (Ord, Eq)
  dLoad = dLoadImpl
  dMake = undefined
  dUpdate = undefined
  dList = S.map LinkIdentifier <$> dListImpl
  dGetId (LinkIdentifier path) = dGetIdImpl path
  dSync = dSyncImpl
  dSyncOne (LinkIdentifier path) = dSyncOneImpl path
  dRemove (LinkIdentifier path) = dRemoveImpl path
  dKind = const Link
  dEntry = view linkEntry
  dIdentify = LinkIdentifier . view linkPath
  dToData = LinkD
