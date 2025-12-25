module Korrvigs.Monad.Remove where

import Control.Lens
import Control.Monad
import Korrvigs.Entry
import Korrvigs.Monad.Class
import Korrvigs.Monad.Metadata
import Korrvigs.Monad.SQL
import Korrvigs.Monad.Sync
import Opaleye

removeDWIM :: (MonadKorrvigs m) => Entry -> m ()
removeDWIM entry = do
  -- If some entries were sub only to this one, remove them also
  subs <- rSelect $ do
    sub <- selectSourcesFor entriesSubTable $ sqlInt4 $ entry ^. entryId
    c <- aggregate count $ selectTargetsFor entriesSubTable sub
    where_ $ c .== sqlInt8 1
    pure sub
  forM_ subs $ loadSql >=> maybe (pure ()) removeDWIM
  -- Remove the entry itself
  updateRefs entry Nothing
  remove entry
