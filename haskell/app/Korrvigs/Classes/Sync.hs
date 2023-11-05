{-# LANGUAGE LambdaCase #-}

module Korrvigs.Classes.Sync (syncClasses, mkMdName) where

import Control.Arrow ((&&&))
import Control.Monad (forM, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Array (array)
import Data.Char
import Data.Graph (Graph, topSort)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Classes.Generated
import Korrvigs.Definition
import Korrvigs.Entry
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

-- Delete a class, making all entities of this class entities of class Entity,
-- and removing the entries associated with the class
doDelete :: MonadIO m => Connection -> FilePath -> Text -> m [(Text, Int64)]
doDelete conn root old = liftIO $ do
  willUpdate <- runSelect conn sel
  void $ mapM (deleteEntry conn root) =<< runSelect conn classEntry
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
          uUpdateWith = \(_, _, uuid_, sub_, query_) ->
            ((), sqlStrictText (name Entity), uuid_, sub_, query_),
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
    classEntry :: Select (Field SqlUuid)
    classEntry = do
      (class_, entry_) <- selectTable classEntryTable
      where_ $ class_ .== sqlStrictText old
      return entry_

toMdChar :: Char -> Char
toMdChar '/' = '_'
toMdChar '#' = '_'
toMdChar c | isAscii c && isSpace c = '_'
toMdChar c | isAscii c && isPrint c = toLower c
toMdChar _ = '_'

mkMdName :: Text -> Text
mkMdName nm = T.map toMdChar nm <> ".md"

syncClassEntry :: MonadIO m => Connection -> FilePath -> Class -> m UUID
syncClassEntry conn root cls = do
  hasEntry <- liftIO $ runSelect conn sqlHasEntry
  case hasEntry of
    uuid : _ -> pure uuid
    [] -> do
      mentry <- liftIO $ runSelect conn sqlFindEntry
      entry <- case mentry of
        entry : _ -> pure entry
        _ -> entry_id <$> newEntry conn root OntologyClass (name cls) md
      void $ liftIO $ runInsert conn $ insEntry entry
      pure entry
  where
    md :: Text
    md = mkMdName $ name cls
    sqlHasEntry :: Select (Field SqlUuid)
    sqlHasEntry = do
      (class_, entry_) <- selectTable classEntryTable
      where_ $ class_ .== sqlStrictText (name cls)
      pure entry_
    sqlFindEntry :: Select (Field SqlUuid)
    sqlFindEntry = do
      (id_, name_, _) <- selectTable entriesTable
      (_, class_, uuid_, sub_, query_) <- selectTable entitiesTable
      where_ $ id_ .== uuid_ .&& isNull sub_ .&& isNull query_
      where_ $ name_ .== sqlStrictText (name cls)
      where_ $ class_ .== sqlStrictText (name OntologyClass)
      pure id_
    insEntry :: UUID -> Insert Int64
    insEntry uuid =
      Insert
        { iTable = classEntryTable,
          iRows = [toFields (name cls, uuid)],
          iReturning = rCount,
          iOnConflict = Nothing
        }

-- Return ids of entities whose class was removed, and the name of their
-- previous class
syncClasses :: MonadIO m => Connection -> FilePath -> m [(Text, Int64)]
syncClasses conn root = do
  (toInsert, toUpdate) <- actionPlan conn
  void $ liftIO $ runInsert conn $ insert toInsert
  void $ forM toUpdate $ liftIO . runUpdate conn . update
  void $ forM allClasses $ syncClassEntry conn root
  mconcat <$> (mapM (doDelete conn root) =<< toDelete conn)
  where
    allClasses :: [Class]
    allClasses = [minBound .. maxBound]
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
