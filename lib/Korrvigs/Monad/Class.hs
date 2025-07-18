{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Monad.Class where

import Conduit (MonadThrow, MonadUnliftIO, throwM)
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Profunctor.Product.Default
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import qualified Database.PostgreSQL.Simple as Simple
import Korrvigs.Entry
import Opaleye hiding (null)

data KorrvigsError
  = KIOError IOException
  | KDuplicateId [(Id, [FilePath])]
  | KCantLoad Id Text
  | KSubCycle [Id]
  | KRelToUnknown Id
  | KIdNotFound Id
  | KMiscError Text
  deriving (Typeable, Show)

instance Exception KorrvigsError

class (MonadIO m, MonadThrow m, MonadUnliftIO m) => MonadKorrvigs m where
  pgSQL :: m Connection
  root :: m FilePath
  calsyncRoot :: m FilePath
  captureRoot :: m FilePath

setupPsql :: (MonadKorrvigs m) => m ()
setupPsql = do
  conn <- pgSQL
  void $ liftIO $ Simple.execute_ conn "SET intervalstyle = 'iso_8601'"

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
addEntry :: (MonadKorrvigs m) => EntryRow -> m Bool
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

newId :: (MonadKorrvigs m) => IdMaker -> m Id
newId = newId' S.empty

newId' :: (MonadKorrvigs m) => Set Id -> IdMaker -> m Id
newId' forbidden idmk = do
  nid <- flip createId idmk $ \candidate ->
    if S.member (MkId candidate) forbidden
      then pure False
      else fmap null $ rSelectOne $ do
        e <- selectTable entriesTable
        where_ $ e ^. sqlEntryName .== sqlStrictText candidate
  pure $ MkId nid

throwMaybe :: (MonadKorrvigs m) => KorrvigsError -> Maybe a -> m a
throwMaybe err = maybe (throwM err) pure

throwEither :: (MonadKorrvigs m, Exception e) => (a -> e) -> Either a b -> m b
throwEither err (Left t) = throwM $ err t
throwEither _ (Right v) = pure v

atomicSQL :: (MonadKorrvigs m) => (Connection -> IO a) -> m a
atomicSQL act = do
  conn <- pgSQL
  liftIO $ withTransaction conn $ act conn

atomicInsert :: (MonadKorrvigs m) => [Insert a] -> m ()
atomicInsert ins = atomicSQL $ forM_ ins . runInsert

catchIO :: (MonadKorrvigs m) => IO a -> m a
catchIO act = do
  r <- liftIO $ catch (Right <$> act) $ pure . Left
  throwEither KIOError r

catchIOWith :: (MonadKorrvigs m) => a -> IO a -> m a
catchIOWith d act =
  liftIO (catch (Right <$> act) $ pure . Left) >>= \case
    Left (_ :: IOException) -> pure d
    Right v -> pure v
