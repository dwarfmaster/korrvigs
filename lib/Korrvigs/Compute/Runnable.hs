module Korrvigs.Compute.Runnable
  ( Executable (..),
    Runnable (..),
    RunArg (..),
    Hash,
    hashRunnable,
    runExecutable,
    runCode,
    runArgs,
    runEnv,
    runStdIn,
    run,
    runInOut,
    runOut,
  )
where

import Conduit
import Control.Arrow ((***))
import Control.Lens
import Control.Monad
import Control.Monad.Writer.Lazy
import qualified Crypto.Hash as Hsh
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Conduit.Process
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Korrvigs.Entry
import Korrvigs.Utils.Crypto
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Temp

type Hash = Hsh.Digest Hsh.SHA256

data Executable
  = Bash
  | SwiProlog
  deriving (Show, Eq, Ord, Bounded, Enum)

data RunArg
  = ArgPlain Text
  | ArgResult Id Text
  | ArgEntry Id
  deriving (Show, Eq, Ord)

data Runnable = Runnable
  { _runExecutable :: Executable,
    _runCode :: Text,
    _runArgs :: [RunArg],
    _runEnv :: Map Text RunArg,
    _runStdIn :: Maybe RunArg
  }
  deriving (Show, Eq, Ord)

makeLenses ''Runnable

runProc :: (MonadIO m) => (RunArg -> m Text) -> FilePath -> Runnable -> m CreateProcess
runProc resolveArg tmp rbl = do
  args <- mapM resolveArg $ rbl ^. runArgs
  let (script, prc) = mkExeProc (rbl ^. runExecutable) args
  let scriptPath = joinPath [tmp, script]
  liftIO $ TIO.writeFile scriptPath $ rbl ^. runCode
  ev' <- M.fromList . fmap (T.pack *** T.pack) <$> liftIO getEnvironment
  ev'' <- mapM resolveArg $ rbl ^. runEnv
  let ev = M.union ev' ev''
  pure $
    prc
      { cwd = Just tmp,
        env = Just $ (T.unpack *** T.unpack) <$> M.toList ev
      }

mkExeProc :: Executable -> [Text] -> (FilePath, CreateProcess)
mkExeProc Bash args = ("code.sh", proc "bash" $ "code.sh" : (T.unpack <$> args))
mkExeProc SwiProlog args = ("code.pl", proc "swipl" $ "code.pl" : "--" : (T.unpack <$> args))

hashRunnable ::
  (MonadFail m) =>
  (Id -> m Hash) ->
  (Id -> Text -> m Hash) ->
  Runnable ->
  m Hash
hashRunnable hashEntry hashComp rbl = fmap doHash . execWriterT $ do
  tell $ buildExe $ rbl ^. runExecutable
  tell sep
  tell $ stringUtf8 $ T.unpack $ rbl ^. runCode
  tell sep
  tell $ int64BE $ toEnum $ length $ rbl ^. runArgs
  tell sep
  forM_ (rbl ^. runArgs) $ \arg -> do
    buildRunArg arg
    tell sep
  tell $ int64BE $ toEnum $ M.size $ rbl ^. runEnv
  tell sep
  forM_ (M.toList $ rbl ^. runEnv) $ \(ev, val) -> do
    tell $ stringUtf8 $ T.unpack ev
    tell sep
    buildRunArg val
    tell sep
  forM_ (rbl ^. runStdIn) $ \stdin -> do
    buildRunArg stdin
    tell sep
  where
    doHash :: Builder -> Hash
    doHash = Hsh.hashlazy . toLazyByteString
    sep :: Builder
    sep = word8 0
    buildExe :: Executable -> Builder
    buildExe Bash = stringUtf8 "bash"
    buildExe SwiProlog = stringUtf8 "swiprolog"
    buildRunArg (ArgPlain txt) = do
      tell $ char8 'p'
      tell $ stringUtf8 $ T.unpack txt
    buildRunArg (ArgResult i cmp) = do
      hash <- lift $ hashComp i cmp
      tell $ char8 'c'
      tell $ stringUtf8 $ T.unpack $ digestToHexa hash
    buildRunArg (ArgEntry i) = do
      hash <- lift $ hashEntry i
      tell $ char8 'e'
      tell $ stringUtf8 $ T.unpack $ digestToHexa hash

run ::
  (MonadUnliftIO m) =>
  Runnable ->
  (FilePath -> RunArg -> m Text) ->
  ConduitT () ByteString m () -> -- stdin
  ConduitT ByteString Void m a -> -- stdout
  ConduitT ByteString Void m b -> -- stderr
  m (ExitCode, a, b)
run rbl resolveArg stdin stdout stderr = withRunInIO $ \runInIO ->
  withSystemTempDirectory "korrvigs" $ \tmp -> do
    prc <- runProc (runInIO . resolveArg tmp) tmp rbl
    runInIO $ sourceProcessWithStreams prc stdin stdout stderr

runInOut ::
  (MonadUnliftIO m) =>
  Runnable ->
  (FilePath -> RunArg -> m Text) ->
  ConduitT () ByteString m () -> -- stdin
  ConduitT ByteString Void m a -> -- stdout
  m (ExitCode, a)
runInOut rbl resolveArg stdin stdout =
  (\(a, b, _) -> (a, b)) <$> run rbl resolveArg stdin stdout sinkNull

runOut ::
  (MonadUnliftIO m, MonadResource m) =>
  Runnable ->
  (FilePath -> RunArg -> m Text) ->
  ConduitT ByteString Void m a -> -- stdout
  m (ExitCode, a)
runOut rbl resolveArg stdout = withRunInIO $ \runInIO -> do
  withSystemTempDirectory "korrvigs" $ \tmp -> runInIO $ case rbl ^. runStdIn of
    Just stdinV -> do
      stdinPath <- resolveArg tmp stdinV
      prc <- liftIO $ runProc (runInIO . resolveArg tmp) tmp rbl
      let stdin = sourceFile $ T.unpack stdinPath
      (\(a, b, _) -> (a, b)) <$> sourceProcessWithStreams prc stdin stdout sinkNull
    Nothing -> do
      prc <- liftIO $ runProc (runInIO . resolveArg tmp) tmp rbl
      sourceProcessWithConsumer prc stdout
