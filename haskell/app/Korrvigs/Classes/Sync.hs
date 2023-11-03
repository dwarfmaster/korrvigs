{-# LANGUAGE LambdaCase #-}

module Korrvigs.Classes.Sync (syncClasses) where

import Control.Arrow ((&&&))
import Control.Monad (forM, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Array (array)
import Data.Graph (Graph, topSort)
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Classes.Generated
import Korrvigs.Schema
import Opaleye hiding (not)

data Status
  = ShouldInsert
  | ShouldUpdate
  | Ok
  deriving (Show, Eq)

classStatus :: MonadIO m => Connection -> Class -> m Status
classStatus conn cls =
  liftIO (runSelect conn query) >>= \case
    [prt] -> pure $ if name (isA cls) == prt then Ok else ShouldUpdate
    _ -> pure ShouldInsert
  where
    query :: Select (Field SqlText)
    query = do
      (name_, parent_) <- selectTable classesTable
      where_ $ name_ .== sqlStrictText (name cls)
      return parent_

-- The classes such that if a class is the parent of another, it appears before
-- in the list.
orderedClasses :: [Class]
orderedClasses = reverse $ toEnum <$> topSort graph
  where
    graph :: Graph
    graph =
      array
        (0, fromEnum (maxBound :: Class))
        [(fromEnum c, [fromEnum (isA c)]) | c <- [minBound .. maxBound]]

-- Returns a list of classes to insert, and a list of classes whose parent
-- needs to be updated, such that the inserts and updates can be done in the
-- order they are in the list.
actionPlan :: MonadIO m => Connection -> m ([Class], [Class])
actionPlan conn = do
  statuted <- mapM (\cls -> classStatus conn cls >>= pure . (cls,)) orderedClasses
  let toInsert = filter ((== ShouldInsert) . snd) statuted
  let toUpdate = filter ((== ShouldUpdate) . snd) statuted
  pure $ (fst <$> toInsert, fst <$> toUpdate)

-- Find classes that are in the table but do not correspond to classes known to
-- this software
toDelete :: MonadIO m => Connection -> m [Text]
toDelete conn =
  filter (not . (`elem` classesName))
    <$> (liftIO $ runSelect conn sql)
  where
    classesName :: [Text]
    classesName = name <$> [minBound .. maxBound]
    sql :: Select (Field SqlText)
    sql = fst <$> selectTable classesTable

-- Delete a class, making all entities of this class entities of class Entity
doDelete :: MonadIO m => Connection -> Text -> m [(Text, Int64)]
doDelete conn old = liftIO $ do
  willUpdate <- runSelect conn sel
  void $ runUpdate conn update
  void $ runDelete conn deleteEntries
  void $ runDelete conn delete
  pure $ (old,) <$> willUpdate
  where
    sel :: Select (Field SqlInt8)
    sel = do
      (id_, class_, _, _, _) <- selectTable entitiesTable
      where_ $ class_ .== sqlStrictText old
      pure id_
    update :: Update Int64
    update =
      Update
        { uTable = entitiesTable,
          uUpdateWith = \(id_, _, uuid_, sub_, query_) ->
            (id_, sqlStrictText (name Entity), uuid_, sub_, query_),
          uWhere = \(_, class_, _, _, _) -> class_ .== sqlStrictText old,
          uReturning = rCount
        }
    deleteEntries :: Delete Int64
    deleteEntries =
      Delete
        { dTable = classEntryTable,
          dWhere = \(class_, _) -> class_ .== sqlStrictText old,
          dReturning = rCount
        }
    delete :: Delete Int64
    delete =
      Delete
        { dTable = classesTable,
          dWhere = \(name_, _) -> name_ .== sqlStrictText old,
          dReturning = rCount
        }

-- TODO if the entry is not set is not set for a class, look for an entry with
-- the right name and class OntologyClass. If one is found, add the pair to
-- classEntryTable, otherwise create a new entry for the class.

-- Return ids of entities whose class was removed, and the name of their
-- previous class
syncClasses :: MonadIO m => Connection -> m [(Text, Int64)]
syncClasses conn = do
  (toInsert, toUpdate) <- actionPlan conn
  void $ liftIO $ runInsert conn $ insert toInsert
  void $ forM toUpdate $ liftIO . runUpdate conn . update
  mconcat <$> (mapM (doDelete conn) =<< toDelete conn)
  where
    insert :: [Class] -> Insert Int64
    insert to =
      Insert
        { iTable = classesTable,
          iRows = (toFields . (name &&& (name . isA))) <$> to,
          iReturning = rCount,
          iOnConflict = Nothing
        }
    update :: Class -> Update Int64
    update to =
      Update
        { uTable = classesTable,
          uUpdateWith = \(name_, _) -> (name_, sqlStrictText $ name $ isA to),
          uWhere = \(name_, _) -> name_ .== sqlStrictText (name to),
          uReturning = rCount
        }
