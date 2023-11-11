module Korrvigs.Web.DB (findEntity, findEntry, findClass) where

import qualified Data.UUID as U
import Korrvigs.Classes
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

findEntity :: EntityRef -> Handler (Maybe Entity)
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

findEntry :: U.UUID -> Handler (Maybe Entry)
findEntry uuid = do
  conn <- pgsql
  res <- Y.liftIO $ O.runSelect conn sql
  pure $ case res of
    [(nm, path)] -> Just $ MkEntry uuid nm path
    _ -> Nothing
  where
    sql :: O.Select (Field O.SqlText, Field O.SqlText)
    sql = do
      (uuid_, name_, notes_) <- O.selectTable entriesTable
      O.where_ $ uuid_ .== O.sqlUUID uuid
      pure $ (name_, notes_)

findClass :: U.UUID -> Handler (Maybe Class)
findClass uuid = do
  conn <- pgsql
  res <- Y.liftIO $ O.runSelect conn $ do
    (class_, entry_) <- O.selectTable classEntryTable
    O.where_ $ entry_ .== O.sqlUUID uuid
    return class_
  pure $ case parse <$> res of
    [Just cls] -> Just cls
    _ -> Nothing
