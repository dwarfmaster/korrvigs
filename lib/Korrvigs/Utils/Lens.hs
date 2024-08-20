module Korrvigs.Utils.Lens where

import Control.Applicative

toMonoid :: (Monoid m) => (a -> m) -> ((a -> Const m a) -> b -> Const m b) -> b -> m
toMonoid inj f x = getConst $ f (Const . inj) x
