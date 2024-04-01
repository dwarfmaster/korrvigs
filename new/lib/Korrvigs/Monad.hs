{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Monad where

import Control.Exception.Base (IOException)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Profunctor.Product.Default
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Korrvigs.Entry
import Opaleye hiding (null)

data KorrvigsError
  = KIOError IOException
  | KDuplicateId Id Text Text
  | KCantLoad Id Text

class (MonadIO m, MonadError KorrvigsError m) => MonadKorrvigs m where
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

throwMaybe :: MonadKorrvigs m => KorrvigsError -> Maybe a -> m a
throwMaybe err = maybe (throwError err) pure

throwEither :: MonadKorrvigs m => (a -> KorrvigsError) -> Either a b -> m b
throwEither err (Left t) = throwError $ err t
throwEither _ (Right v) = pure v

atomicSQL :: MonadKorrvigs m => (Connection -> IO a) -> m a
atomicSQL act = do
  conn <- pgSQL
  liftIO $ withTransaction conn $ act conn
