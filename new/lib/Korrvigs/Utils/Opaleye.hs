module Korrvigs.Utils.Opaleye where

import Data.List (singleton)
import Opaleye hiding (FromField)
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

ap1 :: String -> Field a -> Field b
ap1 f = C.Column . HPQ.FunExpr f . singleton . C.unColumn

ap2 :: String -> Field a -> Field b -> Field c
ap2 f (C.Column a) (C.Column b) = C.Column $ HPQ.FunExpr f [a, b]

ap3 :: String -> Field a -> Field b -> Field c -> Field d
ap3 f (C.Column a) (C.Column b) (C.Column c) = C.Column $ HPQ.FunExpr f [a, b, c]
