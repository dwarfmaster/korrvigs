{-# LANGUAGE ScopedTypeVariables #-}

module Korrvigs.Utils.Opaleye where

import Control.Arrow ((&&&))
import Data.List (singleton)
import Data.Text (Text)
import qualified Data.Text as T
import Opaleye hiding (FromField)
import Opaleye.Experimental.Enum
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

ap1 :: String -> Field a -> Field b
ap1 f = C.Column . HPQ.FunExpr f . singleton . C.unColumn

ap2 :: String -> Field a -> Field b -> Field c
ap2 f (C.Column a) (C.Column b) = C.Column $ HPQ.FunExpr f [a, b]

ap3 :: String -> Field a -> Field b -> Field c -> Field d
ap3 f (C.Column a) (C.Column b) (C.Column c) = C.Column $ HPQ.FunExpr f [a, b, c]

transitiveClosureStep ::
  Select a ->
  (a -> Field b) ->
  (a -> Field b) ->
  Field b ->
  Select (Field b)
transitiveClosureStep sel pi1 pi2 i = do
  a <- sel
  where_ $ pi1 a .== i
  pure $ pi2 a

transitiveClosure ::
  Select a ->
  (a -> Field b) ->
  (a -> Field b) ->
  Field b ->
  Select (Field b)
transitiveClosure sel pi1 pi2 btm =
  withRecursiveDistinct (transitiveClosureStep sel pi1 pi2 btm) $
    transitiveClosureStep sel pi1 pi2

makeSqlMapper :: forall a b. (Bounded a, Enum a, Eq a) => Text -> (a -> String) -> EnumMapper b a
makeSqlMapper sqlType toSql = enumMapper (T.unpack sqlType) fromSql toSql
  where
    sqlMap :: [(String, a)]
    sqlMap = (toSql &&& id) <$> [minBound .. maxBound]
    fromSql :: String -> Maybe a
    fromSql = flip lookup sqlMap
