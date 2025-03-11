{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Link
  ( displayLinkId,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.KindData
import Korrvigs.Link.Sync
import Prelude hiding (readFile)

instance IsKD Link where
  data KDIdentifier Link = LinkIdentifier FilePath
    deriving (Ord, Eq)
  dList _ = S.map LinkIdentifier <$> dListImpl
  dGetId (LinkIdentifier path) = dGetIdImpl path
  dListCompute _ = pure M.empty
  dSync = (fmap (,M.empty) <$>) . dSyncImpl
  dSyncOne (LinkIdentifier path) = (,M.empty) <$> dSyncOneImpl path
  dUpdateMetadata = dUpdateMetadataImpl
  dUpdateParents = dUpdateParentsImpl

displayLinkId :: KDIdentifier Link -> Text
displayLinkId (LinkIdentifier path) = "link:" <> T.pack path
