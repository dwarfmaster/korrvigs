{-# LANGUAGE LambdaCase #-}

module Korrvigs.Entry (lookupEntryByName, lookupEntryByName', newEntity, lookupEntry, newEntry, deleteEntry) where

import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Classes
import qualified Korrvigs.DB as DB
import Korrvigs.Definition
import Korrvigs.Schema
import qualified Korrvigs.Tree as KTree
import Opaleye ((.==))
import qualified Opaleye as O
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as Path

sanitize :: Char -> Char
sanitize c = case generalCategory c of
  Control -> '_'
  Format -> '_'
  LineSeparator -> '_'
  ParagraphSeparator -> '_'
  Surrogate -> '_'
  PrivateUse -> '_'
  NotAssigned -> '_'
  OtherPunctuation -> if c `elem` ['/', '?'] then '_' else c
  _ -> c

sanitizeSubQuery :: String -> String
sanitizeSubQuery = map sanitize

-- Find an entry and its root entity from its class and name
lookupEntryByName :: MonadIO m => Connection -> Class -> Text -> m (Maybe Entry)
lookupEntryByName conn cls nm = liftIO $ do
  res <- O.runSelect conn $ do
    (id_, name_, notes_) <- O.selectTable entriesTable
    (eid_, ecls_) <- DB.rootFor id_
    O.where_ $ name_ .== O.sqlStrictText nm
    O.where_ $ ecls_ .== O.sqlStrictText (name cls)
    return (id_, notes_, eid_)
  pure $ case res of
    [(uuid, notes, i)] ->
      Just $
        MkEntry uuid nm notes $
          MkEntity i cls uuid Nothing Nothing
    _ -> Nothing

-- Same as lookupRootEntity but raises an error on Nothing
lookupEntryByName' :: MonadIO m => Connection -> Class -> Text -> m Entry
lookupEntryByName' conn cls nm =
  fromMaybe (error $ T.unpack $ "No " <> nm <> " of class " <> name cls)
    <$> lookupEntryByName conn cls nm

-- Create a new entity for in an entry given by its UUID. The entry must
-- already exist in the entry table
newEntityInternal ::
  MonadIO m =>
  Connection ->
  FilePath ->
  UUID ->
  Class ->
  Maybe FilePath ->
  Maybe Text ->
  m Int64
newEntityInternal conn root entry cls content query = liftIO $ do
  entityId <-
    O.runInsert conn ins >>= \case
      [i] -> pure i
      _ -> error "Inserting only one row"
  case msub of
    Nothing -> pure ()
    Just (path, sub) ->
      let dest = root </> U.toString entry </> sub
       in Dir.copyFile path dest
  pure entityId
  where
    msub :: Maybe (FilePath, String)
    msub = (id &&& (sanitizeSubQuery . Path.takeFileName)) <$> content
    ins :: O.Insert [Int64]
    ins =
      O.Insert
        { O.iTable = entitiesTable,
          O.iRows =
            [ ( (),
                O.sqlStrictText (name cls),
                O.sqlUUID entry,
                O.maybeToNullable $ O.sqlString . snd <$> msub,
                O.maybeToNullable $ O.sqlStrictText <$> query
              )
            ],
          O.iReturning = O.rReturning $ \(id_, _, _, _, _) -> id_,
          O.iOnConflict = Nothing
        }

-- Create a new entity in given entry, which is assumed to exists in the store.
-- If given a path to a file, this file is copied into the store, and the
-- entity is created as a sub entity of the entry. It returns the id of the
-- newly created entry
newEntity ::
  MonadIO m =>
  Connection ->
  FilePath ->
  Entry ->
  Class ->
  Maybe FilePath ->
  Maybe Text ->
  m Int64
newEntity conn root entry =
  newEntityInternal conn root (entry_id entry)

-- Try to find an entry from its uuid
lookupEntry :: MonadIO m => Connection -> UUID -> m (Maybe Entry)
lookupEntry conn uuid = do
  res <- liftIO $ O.runSelect conn $ do
    (uuid_, name_, notes_) <- O.selectTable entriesTable
    O.where_ $ uuid_ .== O.sqlUUID uuid
    (id_, cls_) <- DB.rootFor uuid_
    pure (name_, notes_, id_, cls_)
  case res of
    [(nm, notes, i, clsName)] ->
      case parse clsName of
        Just cls ->
          pure $
            Just $
              MkEntry uuid nm notes $
                MkEntity i cls uuid Nothing Nothing
        _ -> liftIO (putStrLn "Couldn't parse class") >> pure Nothing
    _ -> liftIO (putStrLn "Request failed") >> pure Nothing

-- Create a new entry, along with a root entity and the relevant directory in
-- the korrvigs tree. Returns the UUID of the newly created entry on success.
newEntry :: MonadIO m => Connection -> FilePath -> Class -> Text -> Text -> m Entry
newEntry conn root cls nm md = liftIO $ do
  uuid <- U4.nextRandom
  void $ O.runInsert conn $ ins uuid
  void $ KTree.createEntry root uuid md
  rootEntity <- newEntityInternal conn root uuid cls Nothing Nothing
  pure $ MkEntry uuid nm (T.unpack md) $ MkEntity rootEntity cls uuid Nothing Nothing
  where
    ins :: UUID -> O.Insert Int64
    ins uuid =
      O.Insert
        { O.iTable = entriesTable,
          O.iRows = [(O.sqlUUID uuid, O.sqlStrictText nm, O.sqlStrictText md)],
          O.iReturning = O.rCount,
          O.iOnConflict = Nothing
        }

-- When deleting, all files need to be moved to trash, then all the entities
-- refering to the entry need to be removed, if the entry is referred in
-- class_entry it need to be removed and finally it needs to be removed from
-- the entries tables
deleteEntry :: MonadIO m => Connection -> FilePath -> UUID -> m ()
deleteEntry conn root uuid = liftIO $ do
  trsh <- Dir.doesDirectoryExist trash
  if not trsh then Dir.createDirectory trash else pure ()
  Dir.renameDirectory dir deletedPath
  void $
    O.runDelete conn $
      O.Delete
        { O.dTable = classEntryTable,
          O.dWhere = \(_, entry_) -> entry_ .== O.sqlUUID uuid,
          O.dReturning = O.rCount
        }
  void $
    O.runDelete conn $
      O.Delete
        { O.dTable = entitiesTable,
          O.dWhere = \(_, _, entry_, _, _) -> entry_ .== O.sqlUUID uuid,
          O.dReturning = O.rCount
        }
  void $
    O.runDelete conn $
      O.Delete
        { O.dTable = entriesTable,
          O.dWhere = \(id_, _, _) -> id_ .== O.sqlUUID uuid,
          O.dReturning = O.rCount
        }
  where
    trash :: FilePath
    trash = root </> "deleted"
    deletedPath :: FilePath
    deletedPath = trash </> U.toString uuid
    dir :: FilePath
    dir = root </> U.toString uuid
