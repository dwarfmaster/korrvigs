module Korrvigs.Web.DB (findEntity) where

import qualified Korrvigs.Classes as Cls
import Korrvigs.Definition
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Opaleye (Field, FieldNullable, (.&&), (.==))
import qualified Opaleye as O
import qualified Yesod as Y

(.==?) :: FieldNullable a -> Maybe (Field a) -> Field O.SqlBool
(.==?) a b = case b of
  Nothing -> O.matchNullable (O.sqlBool True) (\_ -> O.sqlBool False) a
  Just b' -> O.matchNullable (O.sqlBool False) (\a' -> a' .== b') a

findEntity :: Backend a => EntityRef -> Y.HandlerFor a (Maybe Entity)
findEntity (EntityRef uuid sub query) = do
  conn <- pgsql
  res <- Y.liftIO $ O.runSelect conn sql
  pure $ case res of
    [(i, cls)] ->
      -- TODO log on Nothing!
      maybe
        Nothing
        (\cls' -> Just $ MkEntity i cls' uuid sub query)
        (Cls.parse cls)
    _ -> Nothing
  where
    sql :: O.Select (Field O.SqlInt8, Field O.SqlText)
    sql = do
      (i_, cls_, uuid_, sub_, query_) <- O.selectTable entitiesTable
      O.where_ $
        (uuid_ .== O.sqlUUID uuid)
          .&& (sub_ .==? (O.sqlStrictText <$> sub))
          .&& (query_ .==? (O.sqlStrictText <$> query))
      pure $ (i_, cls_)
