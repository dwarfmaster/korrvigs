{-# LANGUAGE LambdaCase #-}

module Korrvigs.Entry (newEntity, newEntry) where

import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Schema
import qualified Korrvigs.Tree as KTree
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
  OtherPunctuation -> if elem c ['/', '?'] then '_' else c
  _ -> c

sanitizeSubQuery :: String -> String
sanitizeSubQuery = map sanitize

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
newEntity conn root entry cls content query = liftIO $ do
  entityId <-
    O.runInsert conn ins >>= \case
      [i] -> pure i
      _ -> error "Inserting only one row"
  case msub of
    Nothing -> pure ()
    Just (path, sub) ->
      let dest = root </> U.toString (entry_id entry) </> sub
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
                O.sqlUUID (entry_id entry),
                O.maybeToNullable $ O.sqlString . snd <$> msub,
                O.maybeToNullable $ O.sqlStrictText <$> query
              )
            ],
          O.iReturning = O.rReturning $ \(id_, _, _, _, _) -> id_,
          O.iOnConflict = Nothing
        }

-- Create a new entry, along with a root entity and the relevant directory in
-- the korrvigs tree. Returns the UUID of the newly created entry on success.
newEntry :: MonadIO m => Connection -> FilePath -> Class -> Text -> Text -> m Entry
newEntry conn root cls nm md = liftIO $ do
  uuid <- U4.nextRandom
  void $ O.runInsert conn $ ins uuid
  void $ KTree.createEntry root uuid md
  let entry = MkEntry uuid nm $ T.unpack md
  void $ newEntity conn root entry cls Nothing Nothing
  pure entry
  where
    ins :: UUID -> O.Insert Int64
    ins uuid =
      O.Insert
        { O.iTable = entriesTable,
          O.iRows = [(O.sqlUUID uuid, O.sqlStrictText nm, O.sqlStrictText md)],
          O.iReturning = O.rCount,
          O.iOnConflict = Nothing
        }
