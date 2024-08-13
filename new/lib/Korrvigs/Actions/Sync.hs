{-# LANGUAGE ScopedTypeVariables #-}

module Korrvigs.Actions.Sync where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Except
import Data.List (find, foldl', singleton)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as Simple
import Korrvigs.Entry
import Korrvigs.File
import Korrvigs.KindData
import Korrvigs.Link
import Korrvigs.Monad
import Korrvigs.Note
import Korrvigs.Utils.Cycle
import Opaleye hiding (not)

loadIDsFor :: forall a m f. (IsKD a, MonadKorrvigs m) => f a -> (KDIdentifier a -> Text) -> m (Map Id [Text])
loadIDsFor _ showId = do
  st <- dList (Nothing :: Maybe a)
  pure . M.fromList . S.toList $ S.map (dGetId &&& singleton . showId) st

loadIDs :: (MonadKorrvigs m) => m (Map Id [Text])
loadIDs = do
  allIDs <-
    sequence
      [ loadIDsFor (Nothing :: Maybe Link) displayLinkId,
        loadIDsFor (Nothing :: Maybe Note) displayNoteId,
        loadIDsFor (Nothing :: Maybe File) displayFileId
      ]
  pure $ M.unionsWith (<>) allIDs

sqlIDs :: (MonadKorrvigs m) => m (Map Id [Text])
sqlIDs = do
  ids <- rSelect $ do
    erow <- selectTable entriesTable
    pure $ erow ^. sqlEntryName
  pure $ M.fromList $ (MkId &&& const []) <$> ids

sync :: (MonadKorrvigs m) => m ()
sync = do
  conn <- pgSQL
  void $ liftIO $ Simple.execute_ conn "truncate entries_sub, entries_ref_to"
  ids <- loadIDs
  let conflict = M.toList $ M.filter ((>= 2) . length) ids
  case conflict of
    [] -> pure ()
    (i, txts) : _ -> throwError $ KDuplicateId i (txts ^?! element 0) (txts ^?! element 1)
  sqls <- sqlIDs
  let toRemove = view _1 <$> M.toList (M.difference sqls ids)
  forM_ toRemove remove
  allrels <-
    sequence
      [ dSync (Nothing :: Maybe Link),
        dSync (Nothing :: Maybe Note),
        dSync (Nothing :: Maybe File)
      ]
  let rels = foldl' M.union M.empty allrels
  let checkRD = checkRelData $ \i -> isJust $ M.lookup i ids
  case foldr (firstJust . checkRD . view _2) Nothing (M.toList rels) of
    Nothing -> pure ()
    Just i -> throwError $ KRelToUnknown i
  case hasCycle (view relSubOf <$> rels) of
    Nothing -> pure ()
    Just cle -> throwError $ KSubCycle cle
  let subBindings = mkBindings $ view relSubOf <$> rels
  let refBindings = mkBindings $ view relRefTo <$> rels
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
