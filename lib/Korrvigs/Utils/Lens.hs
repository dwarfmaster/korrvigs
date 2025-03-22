module Korrvigs.Utils.Lens where

import Control.Applicative

toMonoid :: (Monoid m) => (a -> m) -> ((a -> Const m a) -> b -> Const m b) -> b -> m
toMonoid inj f x = getConst $ f (Const . inj) x

-- Lens pointing at an element of a list, but allow inserting an arbitrary number
-- of elements. The pointed list is empty iff the index is beyond the end of
-- the list
expandAt :: (Functor f) => Int -> ([a] -> f [a]) -> [a] -> f [a]
expandAt _ f [] = f []
expandAt 0 f (x : xs) = (++ xs) <$> f [x]
expandAt i f (x : xs) = (x :) <$> expandAt (i - 1) f xs
