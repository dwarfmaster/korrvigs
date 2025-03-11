{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Link
  ( displayLinkId,
  )
where

import Control.Lens (view)
import qualified Data.Map as M
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
  dLoad = undefined
  dRemoveDB _ = undefined
  dList _ = S.map LinkIdentifier <$> dListImpl
  dGetId (LinkIdentifier path) = dGetIdImpl path
  dListCompute _ = pure M.empty
  dSync = (fmap (,M.empty) <$>) . dSyncImpl
  dSyncOne (LinkIdentifier path) = (,M.empty) <$> dSyncOneImpl path
  dRemove (LinkIdentifier path) = dRemoveImpl path
  dUpdateMetadata = dUpdateMetadataImpl
  dUpdateParents = dUpdateParentsImpl
  dKind = const Link
  dEntry = view linkEntry
  dIdentify = LinkIdentifier . view linkPath
  dToData = LinkD

displayLinkId :: KDIdentifier Link -> Text
displayLinkId (LinkIdentifier path) = "link:" <> T.pack path
