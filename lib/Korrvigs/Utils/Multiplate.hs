{-# LANGUAGE RankNTypes #-}

-- Taken by mostly copying code from the old multiplate crate:
--   https://wiki.haskell.org/Multiplate
--   https://hackage.haskell.org/package/multiplate
module Korrvigs.Utils.Multiplate where

import Control.Applicative
import Control.Monad
import Data.Functor.Constant
import Data.Functor.Identity

type Projector p a = forall f. p f -> a -> f a

class Multiplate p where
  multiplate :: (Applicative f) => p f -> p f
  mkPlate :: (forall a. Projector p a -> a -> f a) -> p f

purePlate :: (Multiplate p, Applicative f) => p f
purePlate = mkPlate $ const pure

emptyPlate :: (Multiplate p, Alternative f) => p f
emptyPlate = mkPlate $ const $ const empty

mChildren :: (Multiplate p, Monoid o) => p (Constant o) -> p (Constant o)
mChildren = multiplate

mapChildren :: (Multiplate p) => p Identity -> p Identity
mapChildren = multiplate

mapChildrenM :: (Multiplate p, Monad m) => p m -> p m
mapChildrenM = multiplate

kleisliSequencePlate :: forall p m. (Multiplate p, Monad m) => p m -> p m -> p m
kleisliSequencePlate f1 f2 = mkPlate build
  where
    build :: Projector p a -> a -> m a
    build proj = proj f1 >=> proj f2

mapFamilyM :: (Multiplate p, Monad m) => p m -> p m
mapFamilyM f = (multiplate $ mapFamilyM f) `kleisliSequencePlate` f

mapFamily :: (Multiplate p) => p Identity -> p Identity
mapFamily = mapFamilyM
