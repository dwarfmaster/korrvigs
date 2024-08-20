{-# LANGUAGE ScopedTypeVariables #-}

module Korrvigs.Utils.Cycle where

import Data.Graph
import Data.Map (Map)
import qualified Data.Map as M

hasCycle :: forall a. (Ord a) => Map a [a] -> Maybe [a]
hasCycle grmap | M.null grmap = Nothing
hasCycle grmap = case foldr1 combine (stronglyConnComp gr) of
  AcyclicSCC _ -> Nothing
  CyclicSCC xs -> Just xs
  where
    combine :: SCC a -> SCC a -> SCC a
    combine (CyclicSCC xs) _ = CyclicSCC xs
    combine (AcyclicSCC _) sc = sc
    gr :: [(a, a, [a])]
    gr = (\(a, b) -> (a, a, b)) <$> M.toList grmap
