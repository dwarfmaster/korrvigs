module Korrvigs.Monad.Remove (removeDWIM) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import qualified Data.Map as M
import Data.Time
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Android
import Korrvigs.Metadata.TH
import Korrvigs.Monad.Class
import Korrvigs.Monad.Metadata
import Korrvigs.Monad.SQL
import Korrvigs.Monad.Sync
import Korrvigs.Utils
import Opaleye

mkMtdt "OrphanedNote" "orphaned" [t|UTCTime|]

removeDWIM :: (MonadKorrvigs m) => Entry -> m ()
removeDWIM entry = do
  -- If some entries were sub only to this one, remove them also
  subs :: [Int] <- rSelect $ do
    sub <- selectSourcesFor entriesSubTable $ sqlInt4 $ entry ^. entryId
    c <- aggregate count $ selectTargetsFor entriesSubTable sub
    where_ $ c .== sqlInt8 1
    pure sub
  tm <- liftIO $ getCurrentTime
  forM_ subs $
    load >=> \case
      Nothing -> pure ()
      Just subEntry
        | kindDataKind (subEntry ^. entryKindData) == Note ->
            updateMetadata subEntry (M.singleton (mtdtSqlName OrphanedNote) (toJSON tm)) []
      Just subEntry -> removeDWIM subEntry
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
