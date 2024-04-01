{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Monad where

import Control.Monad.IO.Class
import Data.Profunctor.Product.Default
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Entry
import Opaleye hiding (null)

class MonadIO m => MonadKorrvigs m where
  pgSQL :: m Connection
  root :: m FilePath
  load :: Id -> m (Maybe Entry)
  remove :: Id -> m ()

rSelect :: (Default FromFields fields haskell, MonadKorrvigs m) => Select fields -> m [haskell]
rSelect query = do
  conn <- pgSQL
  liftIO $ runSelect conn query

rSelectOne :: (Default FromFields fields haskell, MonadKorrvigs m) => Select fields -> m (Maybe haskell)
rSelectOne query =
  rSelect (limit 1 query) >>= \case
    [r] -> pure $ Just r
    _ -> pure Nothing

-- Returns True if the insertion was successful
addEntry :: MonadKorrvigs m => EntryRow -> m Bool
addEntry entry =
  pgSQL >>= \conn -> liftIO $ do
    cnt <-
      runInsert conn $
        Insert
          { iTable = entriesTable,
            iRows = [toFields entry],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    pure $ cnt == 1

newId :: MonadKorrvigs m => IdMaker -> m Id
newId =
  fmap MkId
    . createId
      ( \candidate -> do
          conn <- pgSQL
          r <- liftIO $ runSelect conn $ limit 1 $ do
            EntryRow i _ _ _ _ _ _ <- selectTable entriesTable
            where_ $ i .== sqlStrictText candidate
            pure ()
          pure $ null r
      )
