module Korrvigs.Event.VDirSyncer where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Monad
import qualified Korrvigs.Utils.Git.Status as St
import Korrvigs.Utils.Process
import System.Exit
import System.FilePath
import System.Process

worktreeRoot :: (MonadKorrvigs m) => m FilePath
worktreeRoot = do
  rt <- root
  pure $ joinPath [rt, "../../korrvigs-temp/calsync"]

data VDirStatus
  = VDirtyRepo
  | VDirPullError Text
  | VDirMergeFailed
  | VDirPushError Text
  | VDirMerged

vdirSync :: (MonadKorrvigs m) => m VDirStatus
vdirSync =
  St.gitStatusKorr >>= \case
    _ : _ -> pure VDirtyRepo
    [] ->
      runSilentK (proc "vdirsyncer" ["sync", "nextcloud_calendar_pull"]) >>= \case
        ExitFailure i -> pure $ VDirPullError $ "Pull failed with error code " <> T.pack (show i)
        ExitSuccess -> do
          rt <- root
          runSilentK (proc "git" ["merge", "calsync"]) {cwd = Just rt} >>= \case
            ExitFailure _ -> pure VDirMergeFailed
            ExitSuccess ->
              runSilentK (proc "vdirsyncer" ["sync", "nextcloud_calendar_push"]) >>= \case
                ExitFailure i -> pure $ VDirPushError $ "Push failed with error code " <> T.pack (show i)
                ExitSuccess -> do
                  wkrt <- worktreeRoot
                  void $ runSilentK (proc "git" ["reset", "--force", "main"]) {cwd = Just wkrt}
                  pure VDirMerged
