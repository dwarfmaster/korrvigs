module Korrvigs.Event.VDirSyncer (vdirSync, VDirStatus (..)) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Actions.Sync (processRelData)
import Korrvigs.Event.Sync
import Korrvigs.Monad
import qualified Korrvigs.Utils.Git.Status as St
import Korrvigs.Utils.Process
import System.Exit
import System.FilePath
import System.Process

worktreeRoot :: (MonadKorrvigs m) => m FilePath
worktreeRoot = do
  rt <- root
  pure $ joinPath [rt, "../../korrvigs-temp/calsync/korrvigs"]

syncMsg :: Text
syncMsg = "Pull calendars from nextcloud"

regMsg :: Text
regMsg = "Register new events"

splitICalPath :: FilePath -> (Text, Text)
splitICalPath path = case reverse (splitPath path) of
  ics : calendar : _ -> (T.pack calendar, T.pack ics)
  _ -> ("", "")

data VDirStatus
  = VDirtyRepo
  | VDirNothingToDo
  | VDirMergeFailed
  | VDirMerged
  | VDirError Text

vdirSync :: (MonadKorrvigs m) => m VDirStatus
vdirSync =
  runExceptT vdirImpl >>= \case
    Left e -> pure e
    Right e -> pure e

runS :: (MonadKorrvigs m) => CreateProcess -> ExceptT VDirStatus m ExitCode
runS = lift . runSilentK

onExitFailure :: (MonadKorrvigs m) => ExitCode -> (Int -> ExceptT VDirStatus m ()) -> ExceptT VDirStatus m ()
onExitFailure ExitSuccess _ = pure ()
onExitFailure (ExitFailure code) act = act code

vdirImpl :: (MonadKorrvigs m) => ExceptT VDirStatus m VDirStatus
vdirImpl = do
  rt <- lift root
  wkrt <- lift worktreeRoot

  -- First we check if the repo is dirty, we do not continue if this is the case
  st <- lift St.gitStatusKorr
  unless (null st) $ throwError VDirtyRepo

  -- We save the starting commit
  mainCiRaw <- liftIO $ readCreateProcess ((proc "git" ["rev-parse", "main"]) {cwd = Just rt}) ""
  let mainCi = reverse $ dropWhile isSpace $ reverse mainCiRaw
  gitRoot <- liftIO $ readCreateProcess ((proc "git" ["rev-parse", "--show-toplevel"]) {cwd = Just rt}) ""

  -- When pulling, we abort if there is nothing new to commit
  pull <- runS (proc "vdirsyncer" ["sync", "nextcloud_calendars_pull"])
  onExitFailure pull $ \i -> throwError $ VDirError $ "Pull failed with error code " <> T.pack (show i)
  gstatus <- liftIO $ St.gitStatus wkrt
  case gstatus of
    Left err -> throwError $ VDirError err
    Right status -> when (null status) $ throwError VDirNothingToDo
  void $ runS (proc "git" ["add", joinPath [wkrt, "events"]]) {cwd = Just wkrt}
  void $ runS (proc "git" ["commit", "-m", T.unpack syncMsg]) {cwd = Just wkrt}

  -- We merge on main
  gmerge <- runS (proc "git" ["merge", "calsync"]) {cwd = Just rt}
  onExitFailure gmerge $ const $ throwError VDirMergeFailed

  -- We register the new calendar entries and commit the changes
  changedFilesRaw <- liftIO $ readCreateProcess ((proc "git" ["diff", "--name-only", "main", mainCi]) {cwd = Just rt}) ""
  let changedFiles = (\f -> splitICalPath $ joinPath [gitRoot, f]) <$> lines changedFilesRaw
  forM_ changedFiles $ \(cal, ics) -> do
    (i, ical, ievent, new) <- lift $ register (cal, ics)
    when new $ lift $ syncOneEvent i cal ics ical ievent >>= processRelData i
  newReg <- lift St.gitStatusKorr
  unless (null newReg) $ do
    void $ runS (proc "git" ["add", joinPath [rt, "events"]]) {cwd = Just rt}
    void $ runS (proc "git" ["commit", "-m", T.unpack regMsg]) {cwd = Just rt}

  -- We sync the value of calsync branch to main and push to nextcloud
  void $ runS (proc "git" ["reset", "--hard", "main"]) {cwd = Just wkrt}
  push <- runS (proc "vdirsyncer" ["sync", "nextcloud_calendars_push"])
  onExitFailure push $ \i -> throwError $ VDirError $ "Push failed with error code " <> T.pack (show i)
  pure VDirMerged
