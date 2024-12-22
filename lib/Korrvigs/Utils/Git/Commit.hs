module Korrvigs.Utils.Git.Commit where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Monad
import System.IO
import System.Process

data CommitStatus
  = CiDeleted
  | CiAdded
  deriving (Eq, Ord, Show)

data CommitData = CiData
  { _ciMessage :: Text,
    _ciFiles :: [(FilePath, CommitStatus)]
  }
  deriving (Eq, Ord, Show)

makeLenses ''CommitData

runGitIn :: FilePath -> [String] -> IO ()
runGitIn rt args = do
  devNull <- openFile "/dev/null" WriteMode
  let git =
        (proc "git" args)
          { std_out = UseHandle devNull,
            std_err = UseHandle devNull,
            cwd = Just rt
          }
  (_, _, _, prc) <- createProcess git
  void $ waitForProcess prc
  hClose devNull

gitRm :: FilePath -> FilePath -> IO ()
gitRm rt file = runGitIn rt ["rm", file]

gitAdd :: FilePath -> FilePath -> IO ()
gitAdd rt file = runGitIn rt ["add", file]

gitCommit :: FilePath -> CommitData -> IO ()
gitCommit rt ci = do
  forM_ (ci ^. ciFiles) $ \(file, status) -> case status of
    CiDeleted -> gitRm rt file
    CiAdded -> gitAdd rt file
  runGitIn rt ["commit", "-m", T.unpack $ ci ^. ciMessage]

gitCommitKorr :: (MonadKorrvigs m) => CommitData -> m ()
gitCommitKorr ci = do
  rt <- root
  liftIO $ gitCommit rt ci
