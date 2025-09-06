module Korrvigs.Monad.Remove where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S
import Korrvigs.Compute
import Korrvigs.Compute.Action
import Korrvigs.Entry
import Korrvigs.Monad.Class
import Korrvigs.Monad.Metadata
import Korrvigs.Monad.SQL
import Korrvigs.Monad.Sync
import Korrvigs.Utils
import Opaleye

removeDWIM :: (MonadKorrvigs m) => Entry -> m ()
removeDWIM entry = do
  -- Remove references from others
  subs <- rSelect $ selectSourcesFor entriesSubTable $ sqlInt4 $ entry ^. entryId
  refs <- rSelect $ selectSourcesFor entriesRefTable $ sqlInt4 $ entry ^. entryId
  forM_ (S.fromList subs <> S.fromList refs) $
    loadSql >=> \case
      Nothing -> pure ()
      Just other -> updateRef other (entry ^. entryName) Nothing
  -- If some entries were sub only to this one, remove them also
  forM_ subs $ \sub -> do
    subOf <- rSelect $ selectTargetsFor entriesSubTable $ sqlInt4 sub
    let n = length (subOf :: [Int])
    when (n == 0) $ loadSql sub >>= maybe (pure ()) removeDWIM
  -- Remove computations
  comps <- rSelect $ do
    cmp <- selectTable computationsTable
    where_ $ cmp ^. sqlCompEntry .== sqlInt4 (entry ^. entryId)
    pure (cmp ^. sqlCompName, cmp ^. sqlCompAction)
  rt <- rootDir
  forM_ comps $ \cmp -> do
    file <- rootFile (entry ^. entryName) (cmp ^. _1) (cmp ^. _2)
    recursiveRemoveFile rt file
  conn <- pgSQL
  void $
    liftIO $
      runDelete conn $
        Delete
          { dTable = computationsTable,
            dWhere = \cmp -> cmp ^. sqlCompEntry .== sqlInt4 (entry ^. entryId),
            dReturning = rCount
          }
  -- Remove the entry itself
  remove entry
