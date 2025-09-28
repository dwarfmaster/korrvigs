module Korrvigs.Compute.Runnable
  ( Executable (..),
    Runnable (..),
    runExecutable,
    runCode,
    runDeterministic,
    runArgs,
    runEnv,
    run,
    runInOut,
    runOut,
  )
where

import Conduit
import Control.Arrow ((***))
import Control.Lens
import Data.ByteString (ByteString)
import Data.Conduit.Process
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Temp

data Executable
  = Bash
  | SwiProlog
  deriving (Show, Eq, Ord, Bounded, Enum)

data Runnable = Runnable
  { _runExecutable :: Executable,
    _runCode :: Text,
    _runDeterministic :: Bool,
    _runArgs :: [Text],
    _runEnv :: Map Text Text
  }
  deriving (Show, Eq, Ord)

makeLenses ''Runnable

runProc :: FilePath -> Runnable -> IO CreateProcess
runProc tmp rbl = do
  let (script, prc) = mkExeProc (rbl ^. runExecutable) (rbl ^. runArgs)
  let scriptPath = joinPath [tmp, script]
  TIO.writeFile scriptPath $ rbl ^. runCode
  ev' <- M.fromList . fmap (T.pack *** T.pack) <$> getEnvironment
  let ev = M.union ev' $ rbl ^. runEnv
  pure $
    prc
      { cwd = Just tmp,
        env = Just $ (T.unpack *** T.unpack) <$> M.toList ev
      }

mkExeProc :: Executable -> [Text] -> (FilePath, CreateProcess)
mkExeProc Bash args = ("code.sh", proc "bash" $ "code.sh" : (T.unpack <$> args))
mkExeProc SwiProlog args = ("code.pl", proc "swipl" $ "code.pl" : "--" : (T.unpack <$> args))

run ::
  (MonadUnliftIO m) =>
  Runnable ->
  ConduitT () ByteString m () -> -- stdin
  ConduitT ByteString Void m a -> -- stdout
  ConduitT ByteString Void m b -> -- stderr
  m (ExitCode, a, b)
run rbl stdin stdout stderr = withRunInIO $ \runInIO ->
  withSystemTempDirectory "korrvigs" $ \tmp -> do
    prc <- liftIO $ runProc tmp rbl
    runInIO $ sourceProcessWithStreams prc stdin stdout stderr

runInOut ::
  (MonadUnliftIO m) =>
  Runnable ->
  ConduitT () ByteString m () -> -- stdin
  ConduitT ByteString Void m a -> -- stdout
  m (ExitCode, a)
runInOut rbl stdin stdout = (\(a, b, _) -> (a, b)) <$> run rbl stdin stdout sinkNull

runOut ::
  (MonadUnliftIO m) =>
  Runnable ->
  ConduitT ByteString Void m a -> -- stdout
  m (ExitCode, a)
runOut rbl stdout = withRunInIO $ \runInIO -> do
  withSystemTempDirectory "korrvigs" $ \tmp -> do
    prc <- liftIO $ runProc tmp rbl
    runInIO $ sourceProcessWithConsumer prc stdout
