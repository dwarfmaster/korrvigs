module Korrvigs.Utils.Process where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import Korrvigs.Monad.Class
import System.Exit
import System.IO
import System.Process

withDevNull :: (Handle -> IO a) -> IO a
withDevNull act = do
  devNull <- openFile "/dev/null" WriteMode
  r <- act devNull
  hClose devNull
  pure r

runSilent :: CreateProcess -> IO ExitCode
runSilent prc = withDevNull $ \devNull -> do
  let process = prc {std_out = UseHandle devNull, std_err = UseHandle devNull}
  (_, _, _, p) <- createProcess process
  waitForProcess p

runSilentK :: (MonadKorrvigs m) => CreateProcess -> m ExitCode
runSilentK = liftIO . runSilent

runStdout :: CreateProcess -> IO (ExitCode, LBS.ByteString)
runStdout prc = withDevNull $ \devNull -> do
  let process = prc {std_out = CreatePipe, std_err = UseHandle devNull}
  (_, Just out, _, p) <- createProcess process
  content <- LBS.hGetContents out
  r <- waitForProcess p
  pure (r, content)
