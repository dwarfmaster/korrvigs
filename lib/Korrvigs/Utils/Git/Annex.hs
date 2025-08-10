module Korrvigs.Utils.Git.Annex where

import Control.Monad.IO.Class
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Utils.Process
import System.Directory
import System.FilePath
import System.Process

isAnnexedFile :: (MonadIO m) => FilePath -> FilePath -> m Bool
isAnnexedFile root file = do
  let gitdir = (proc "git" ["rev-parse", "--git-dir"]) {cwd = Just root}
  (_, dir) <- liftIO $ runStdout gitdir
  sym <- liftIO $ pathIsSymbolicLink file
  if sym
    then do
      target <- liftIO $ getSymbolicLinkTarget file
      pure $ isRelative $ makeRelative (LT.unpack $ LEnc.decodeUtf8 dir) target
    else pure False
