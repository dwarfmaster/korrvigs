module Korrvigs.Actions.Sync where

import Conduit (throwM)
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.List (find, foldl', singleton)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import qualified Database.PostgreSQL.Simple as Simple
import Korrvigs.Actions.Remove
import Korrvigs.Actions.SQL
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Compute (EntryComps, syncComputations)
import Korrvigs.Entry
import qualified Korrvigs.Event.Sync as Event
import qualified Korrvigs.File.Sync as File
import Korrvigs.Kind
import Korrvigs.KindData
import qualified Korrvigs.Link.Sync as Link
import Korrvigs.Monad
import qualified Korrvigs.Note.Sync as Note
import Korrvigs.Utils.Cycle
import Korrvigs.Utils.Time (measureTime, measureTime_)
import Opaleye hiding (not, null)
import Prelude hiding (putStrLn)

loadIDsFor :: forall m. (MonadKorrvigs m) => Text -> (FilePath -> Id) -> m (Set FilePath) -> m (Map Id [FilePath])
loadIDsFor kdTxt extractId doList = do
  (tm, st) <- measureTime doList
  liftIO $ putStrLn $ kdTxt <> ": listed " <> T.pack (show $ S.size st) <> " in " <> tm
  pure . M.fromListWith (<>) . S.toList $ S.map (extractId &&& singleton) st

loadIDsOn :: (MonadKorrvigs m) => Kind -> m (Map Id [FilePath])
loadIDsOn Link = loadIDsFor (displayKind Link) Link.linkIdFromPath Link.list
loadIDsOn Note = loadIDsFor (displayKind Note) Note.noteIdFromPath Note.list
loadIDsOn File = loadIDsFor (displayKind File) File.fileIdFromPath File.list
loadIDsOn Event = loadIDsFor (displayKind Event) (fst . Event.eventIdFromPath) Event.list
loadIDsOn Calendar = loadIDsFor (displayKind Calendar) Cal.calIdFromPath Cal.list

loadIDs :: (MonadKorrvigs m) => m (Map Id [FilePath])
loadIDs = do
  allIDs <- mapM loadIDsOn [minBound .. maxBound]
  pure $ M.unionsWith (<>) allIDs

sqlIDs :: (MonadKorrvigs m) => m (Map Id [Text])
sqlIDs = do
  ids <- rSelect $ do
    erow <- selectTable entriesTable
    pure $ erow ^. sqlEntryName
  pure $ M.fromList $ (MkId &&& const []) <$> ids

processRelData :: (MonadKorrvigs m) => Id -> RelData -> m ()
processRelData i rd = do
  atomicSQL $ \conn -> do
    void $
      liftIO $
        runDelete conn $
          Delete
            { dTable = entriesSubTable,
              dWhere = \sb -> sb ^. source .== sqlId i,
              dReturning = rCount
            }
    void $
      liftIO $
        runDelete conn $
          Delete
            { dTable = entriesRefTable,
              dWhere = \sb -> sb ^. source .== sqlId i,
              dReturning = rCount
            }
  let subsOf = (i,) <$> rd ^. relSubOf
  let refsTo = (i,) <$> rd ^. relRefTo
  atomicInsert $ insertSubOf subsOf <> insertRefTo refsTo

runSync :: (IsKD a, MonadKorrvigs m) => Text -> f a -> m (Map Id (RelData, EntryComps))
runSync kdTxt kd = do
  (tm, r) <- measureTime $ dSync kd
  liftIO $ putStrLn $ kdTxt <> ": synced " <> T.pack (show $ M.size r) <> " in " <> tm
  pure r

runSyncOn :: (MonadKorrvigs m) => Kind -> m (Map Id (RelData, EntryComps))
runSyncOn Link = runSync (displayKind Link) (Nothing :: Maybe Link)
runSyncOn Note = runSync (displayKind Note) (Nothing :: Maybe Note)
runSyncOn File = runSync (displayKind File) (Nothing :: Maybe File)
runSyncOn Event = runSync (displayKind Event) (Nothing :: Maybe Event)
runSyncOn Calendar = runSync (displayKind Calendar) (Nothing :: Maybe Calendar)

sync :: (MonadKorrvigs m) => m ()
sync = do
  conn <- pgSQL
  void $ liftIO $ Simple.execute_ conn "truncate entries_sub, entries_ref_to"
  ids <- loadIDs
  let conflict = M.toList $ M.filter ((>= 2) . length) ids
  unless (null conflict) $ throwM $ KDuplicateId conflict
  sqls <- sqlIDs
  let toRemove = view _1 <$> M.toList (M.difference sqls ids)
  rmT <- measureTime_ $ forM_ toRemove $ \i -> do
    entry <- load i
    forM_ entry remove
  liftIO $ putStrLn $ "Removed " <> T.pack (show $ length toRemove) <> " entries in " <> rmT
  allrels <- mapM runSyncOn [minBound .. maxBound]
  let rels = foldl' M.union M.empty allrels
  cmpT <- measureTime_ $ forM_ (M.toList rels) $ \(i, (_, cmps)) -> syncComputations i cmps
  liftIO $ putStrLn $ "Synced " <> T.pack (show $ M.size rels) <> " computations in " <> cmpT
  let checkRD = checkRelData $ \i -> isJust $ M.lookup i ids
  case foldr (firstJust . checkRD . view (_2 . _1)) Nothing (M.toList rels) of
    Nothing -> pure ()
    Just i -> throwM $ KRelToUnknown i
  case hasCycle (view (_1 . relSubOf) <$> rels) of
    Nothing -> pure ()
    Just cle -> throwM $ KSubCycle cle
  let subBindings = mkBindings $ view (_1 . relSubOf) <$> rels
  let refBindings = mkBindings $ view (_1 . relRefTo) <$> rels
  atomicInsert $ insertSubOf subBindings <> insertRefTo refBindings
  where
    mkBindings :: Map a [a] -> [(a, a)]
    mkBindings m = (\(a, l) -> (a,) <$> l) =<< M.toList m
    checkRelData :: (Id -> Bool) -> RelData -> Maybe Id
    checkRelData check rd =
      firstJust
        (find (not . check) $ rd ^. relSubOf)
        (find (not . check) $ rd ^. relRefTo)
    firstJust :: Maybe a -> Maybe a -> Maybe a
    firstJust (Just x) _ = Just x
    firstJust Nothing x = x
