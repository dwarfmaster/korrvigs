module Korrvigs.Web.DB (findEntity, findEntry, findClass, createEntry, (.==?)) where

import Data.Text (Text)
import qualified Data.UUID as U
import Korrvigs.Classes
import qualified Korrvigs.Classes as Cls
import Korrvigs.Classes.Sync (mkMdName)
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Schema
import qualified Korrvigs.Tree as Tree
import Korrvigs.Web.Backend
import Opaleye (Field, FieldNullable, (.&&), (.==))
import qualified Opaleye as O
import qualified Yesod as Y

(.==?) :: FieldNullable a -> Maybe (Field a) -> Field O.SqlBool
(.==?) a b = case b of
  Nothing -> O.matchNullable (O.sqlBool True) (\_ -> O.sqlBool False) a
  Just b' -> O.matchNullable (O.sqlBool False) (.== b') a

findEntity :: EntityRef -> Handler (Maybe Entity)
findEntity (EntityRef uuid sub query) = do
  conn <- pgsql
  res <- Y.liftIO $ O.runSelect conn sql
  pure $ case res of
    [(i, cls)] ->
      -- TODO log on Nothing!
      MkEntity i
        <$> Cls.parse cls
        <*> pure uuid
        <*> pure sub
        <*> pure query
    _ -> Nothing
  where
    sql :: O.Select (Field O.SqlInt8, Field O.SqlText)
    sql = do
      (i_, cls_, uuid_, sub_, query_) <- O.selectTable entitiesTable
      O.where_ $
        (uuid_ .== O.sqlUUID uuid)
          .&& (sub_ .==? (O.sqlStrictText <$> sub))
          .&& (query_ .==? (O.sqlStrictText <$> query))
      pure (i_, cls_)

findEntry :: U.UUID -> Handler (Maybe Entry)
findEntry uuid = pgsql >>= flip lookupEntry uuid

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

createEntry :: Class -> Text -> Text -> Handler Entry
createEntry cls nm desc = do
  conn <- pgsql
  root <- korrRoot
  let mdName = mkMdName nm
  entry <- Y.liftIO $ newEntry conn root cls nm mdName
  Tree.writeNotes root entry desc
  pure entry
