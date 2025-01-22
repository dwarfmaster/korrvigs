module Korrvigs.Utils.Git.Annex where

import Control.Monad
import Control.Monad.IO.Class
import Korrvigs.Utils.Process
import System.Exit
import System.Process

-- Add a file to the annex: call git annex add then unstage it
annexAdd :: (MonadIO m) => FilePath -> FilePath -> m ()
annexAdd root file = do
  let gannex = (proc "git" ["annex", "add", file]) {cwd = Just root}
  result <- liftIO $ runSilent gannex
  when (result == ExitSuccess) $ do
    let gunstage = (proc "git" ["restore", "--staged", file]) {cwd = Just root}
    void $ liftIO $ runSilent gunstage
