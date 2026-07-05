module Korrvigs.Monad.Remove (removeDWIM) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Android
import Korrvigs.Monad.Class
import Korrvigs.Monad.Metadata
import Korrvigs.Monad.SQL
import Korrvigs.Monad.Sync
import Korrvigs.Utils
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
  -- Hooks
  androidFileRemoveHook entry
  -- Remove the entry itself
  updateRefs entry Nothing
  remove entry

-- When removing a file from android, add it to the ignored list
androidFileRemoveHook :: (MonadKorrvigs m) => Entry -> m ()
androidFileRemoveHook entry = fromMaybeT () $ do
  phone <- fmap MkId $ hoistLift $ rSelectMtdt FromAndroid $ sqlId $ entry ^. entryName
  path <- hoistLift $ rSelectMtdt FromAndroidPath $ sqlId $ entry ^. entryName
  phoneEntry <- hoistLift $ load phone
  lift $ ignoreAndroidPath phoneEntry path
