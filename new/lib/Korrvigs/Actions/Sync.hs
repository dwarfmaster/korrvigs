{-# LANGUAGE ScopedTypeVariables #-}

module Korrvigs.Actions.Sync where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Except
import Data.List (singleton)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.KindData
import Korrvigs.Link
import Korrvigs.Monad
import Opaleye

loadIDsFor :: forall a m f. (IsKD a, MonadKorrvigs m) => f a -> (KDIdentifier a -> Text) -> m (Map Id [Text])
loadIDsFor _ showId = do
  st <- dList (Nothing :: Maybe a)
  pure . M.fromList . S.toList $ S.map (dGetId &&& singleton . showId) st

loadIDs :: MonadKorrvigs m => m (Map Id [Text])
loadIDs = do
  allIDs <-
    sequence
      [ loadIDsFor (Nothing :: Maybe Link) displayLinkId
      ]
  pure $ M.unionsWith (<>) allIDs

sqlIDs :: MonadKorrvigs m => m (Map Id [Text])
sqlIDs = do
  ids <- rSelect $ do
    erow <- selectTable entriesTable
    pure $ erow ^. sqlEntryName
  pure $ M.fromList $ (MkId &&& const []) <$> ids

sync :: MonadKorrvigs m => m ()
sync = do
  ids <- loadIDs
  let conflict = M.toList $ M.filter ((>= 2) . length) ids
  case conflict of
    [] -> pure ()
    (i, txts) : _ -> throwError $ KDuplicateId i (txts ^?! element 0) (txts ^?! element 1)
  sqls <- sqlIDs
  let toRemove = view _1 <$> M.toList (M.difference sqls ids)
  forM_ toRemove remove
  sequence_
    [ dSync (Nothing :: Maybe Link)
    ]
