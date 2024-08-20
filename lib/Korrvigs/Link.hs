{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Link
  ( LinkMaker (..),
    lkId,
    lkProtocol,
    lkLink,
    lkMtdt,
    lmk,
    newLink,
    displayLinkId,
  )
where

import Control.Lens (view)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Link.Sync
import Prelude hiding (readFile)

instance IsKD Link where
  data KDIdentifier Link = LinkIdentifier FilePath
    deriving (Ord, Eq)
  dLoad = dLoadImpl
  dRemoveDB _ = dRemoveDBImpl
  dList _ = S.map LinkIdentifier <$> dListImpl
  dGetId (LinkIdentifier path) = dGetIdImpl path
  dSync = dSyncImpl
  dSyncOne (LinkIdentifier path) = dSyncOneImpl path
  dRemove (LinkIdentifier path) = dRemoveImpl path
  dKind = const Link
  dEntry = view linkEntry
  dIdentify = LinkIdentifier . view linkPath
  dToData = LinkD

displayLinkId :: KDIdentifier Link -> Text
displayLinkId (LinkIdentifier path) = "link:" <> T.pack path
