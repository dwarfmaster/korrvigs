module Korrvigs.Utils.Opaleye where

import Control.Arrow ((&&&))
import Data.List (singleton)
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text)
import qualified Data.Text as T
import Opaleye hiding (FromField)
import Opaleye.Experimental.Enum
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.Operators as O

ap1 :: String -> Field_ n a -> Field_ n b
ap1 f = C.Column . HPQ.FunExpr f . singleton . C.unColumn

ap2 :: String -> Field a -> Field b -> Field c
ap2 f (C.Column a) (C.Column b) = C.Column $ HPQ.FunExpr f [a, b]

ap3 :: String -> Field a -> Field b -> Field c -> Field d
ap3 f (C.Column a) (C.Column b) (C.Column c) = C.Column $ HPQ.FunExpr f [a, b, c]

sel1 :: String -> Field_ n a -> Select (Field_ n' b)
sel1 f (C.Column a) = O.relationValuedExpr $ const $ HPQ.FunExpr f [a]

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

connectedComponentGraph ::
  (Default Binaryspec a a) =>
  Select a ->
  (a -> Field b) ->
  (a -> Field b) ->
  (a -> Select (Field SqlBool)) ->
  Select a ->
  Select a
connectedComponentGraph sel pi1 pi2 check st =
  withRecursiveDistinct st $ \r -> do
    a <- sel
    where_ $ pi1 a .== pi1 r .|| pi1 a .== pi2 r .|| pi2 a .== pi1 r .|| pi2 a .== pi2 r
    where_ =<< check a
    pure a

makeSqlMapper :: forall a b. (Bounded a, Enum a, Eq a) => Text -> (a -> String) -> EnumMapper b a
makeSqlMapper sqlType toSql = enumMapper (T.unpack sqlType) fromSql toSql
  where
    sqlMap :: [(String, a)]
    sqlMap = (toSql &&& id) <$> [minBound .. maxBound]
    fromSql :: String -> Maybe a
    fromSql = flip lookup sqlMap

levenshteinLE :: Field SqlText -> Field SqlText -> Field SqlInt4 -> Field SqlInt4
levenshteinLE = ap3 "levenshtein_less_equal"

sqlCast :: forall n a t. (IsSqlType t) => Field_ n a -> FieldNullable t
sqlCast = C.Column . HPQ.CastExpr (showSqlType proxy) . C.unColumn
  where
    proxy :: Maybe t
    proxy = Nothing

sqlArrayLength :: Field (SqlArray a) -> Field SqlInt4 -> Field SqlInt4
sqlArrayLength = ap2 "array_length"
