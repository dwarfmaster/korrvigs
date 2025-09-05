module Korrvigs.Monad.Utils where

import Data.Profunctor.Product.Default
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Monad.Class
import Opaleye

genSqlLoad ::
  forall w sql hs kd m.
  (Default FromFields sql hs, Default Unpackspec sql sql, MonadKorrvigs m) =>
  Table w sql ->
  (sql -> Field SqlInt4) ->
  (hs -> Entry -> kd) ->
  Int ->
  ((Entry -> kd) -> Entry) ->
  m (Maybe Entry)
genSqlLoad tbl getId fromRow i cstr = do
  sel <- rSelectOne $ do
    row <- selectTable tbl
    where_ $ getId row .== sqlInt4 i
    pure row
  case (sel :: Maybe hs) of
    Nothing -> pure Nothing
    Just row -> pure $ Just $ cstr $ fromRow row

genSqlRemove :: Table sql sql -> (sql -> Field SqlInt4) -> Int -> [Delete Int64]
genSqlRemove tbl getId i =
  [ Delete
      { dTable = tbl,
        dWhere = \row -> getId row .== sqlInt4 i,
        dReturning = rCount
      }
  ]
