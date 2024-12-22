module Korrvigs.Utils.Process where

import Control.Monad.IO.Class
import Korrvigs.Monad
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
