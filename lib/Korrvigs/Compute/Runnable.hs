module Korrvigs.Compute.Runnable
  ( Executable (..),
    Runnable (..),
    RunArg (..),
    Hash,
    hashRunnable,
    runExecutable,
    runCode,
    runType,
    runArgs,
    runEnv,
    runStdIn,
    runDeps,
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
import Korrvigs.Compute.Type
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
  | PlainJson
  | PlainCsv
  | PlainText
  | Graphviz
  | CLang
  | CPPLang
  deriving (Show, Eq, Ord, Bounded, Enum)

data RunArg
  = ArgPlain Text
  | ArgResult Id Text
  | ArgResultSame Text
  | ArgEntry Id
  deriving (Show, Eq, Ord)

data Runnable = Runnable
  { _runExecutable :: Executable,
    _runCode :: Text,
    _runType :: RunnableType,
    _runArgs :: [RunArg],
    _runEnv :: Map Text RunArg,
    _runStdIn :: Maybe RunArg
  }
  deriving (Show, Eq, Ord)

makeLenses ''Runnable
makePrisms ''RunArg

runDeps :: Runnable -> [(Id, Text)]
runDeps rbl =
  rbl
    ^.. ( (runArgs . each . _ArgResult)
            <> (runEnv . each . _ArgResult)
            <> (runStdIn . _Just . _ArgResult)
        )

data ExeProc = ExeProc
  { _exeCode :: FilePath,
    _exeCompileScript :: Maybe Text,
    _exeRun :: CreateProcess
  }

makeLenses ''ExeProc

noCompile :: FilePath -> CreateProcess -> ExeProc
noCompile code = ExeProc code Nothing

-- The first returned is an optional compilation step
runProc ::
  (MonadIO m) =>
  (RunArg -> m Text) ->
  FilePath ->
  Runnable ->
  m (Maybe CreateProcess, CreateProcess)
runProc resolveArg tmp rbl = do
  args <- mapM resolveArg $ rbl ^. runArgs
  let exePrc = mkExeProc (rbl ^. runExecutable) args (rbl ^. runType)
  let scriptPath = joinPath [tmp, exePrc ^. exeCode]
  liftIO $ TIO.writeFile scriptPath $ rbl ^. runCode
  ev' <- M.fromList . fmap (T.pack *** T.pack) <$> liftIO getEnvironment
  ev'' <- mapM resolveArg $ rbl ^. runEnv
  let ev = M.union ev' ev''
  let runWithEnv = Just $ (T.unpack *** T.unpack) <$> M.toList ev
  compilePrc <- forM (exePrc ^. exeCompileScript) $ \script -> do
    liftIO $ TIO.writeFile (tmp </> "compile.sh") script
    pure $
      (proc "bash" ["compile.sh"])
        { cwd = Just tmp,
          env = runWithEnv
        }
  pure
    ( compilePrc,
      (exePrc ^. exeRun)
        { cwd = Just tmp,
          env = runWithEnv
        }
    )

mkExeProc :: Executable -> [Text] -> RunnableType -> ExeProc
mkExeProc Bash args _ =
  noCompile "code.sh" $ proc "bash" $ "code.sh" : (T.unpack <$> args)
mkExeProc SwiProlog args _ =
  noCompile "code.pl" $ proc "swipl" $ "code.pl" : "--" : (T.unpack <$> args)
mkExeProc PlainJson _ _ =
  noCompile "data.json" $ proc "cat" ["data.json"]
mkExeProc PlainCsv _ _ =
  noCompile "data.csv" $ proc "cat" ["data.csv"]
mkExeProc PlainText _ _ =
  noCompile "data.txt" $ proc "cat" ["data.txt"]
mkExeProc Graphviz _ tp = noCompile "data.dot" $ case tp of
  ScalarImage -> proc "dot" ["-Tjpg", "data.dot"]
  ScalarGraphic -> proc "dot" ["-Tpng", "data.dot"]
  VectorGraphic -> proc "dot" ["-Tsvg", "data.dot"]
  _ -> proc "false" []
mkExeProc CLang args _ = mkCLikeBuildScript "gcc -std=c23" "code.c" args
mkExeProc CPPLang args _ = mkCLikeBuildScript "g++ -std=c++23" "code.cpp" args

mkCLikeBuildScript :: Text -> FilePath -> [Text] -> ExeProc
mkCLikeBuildScript gcc code args =
  ExeProc
    { _exeCode = code,
      _exeCompileScript =
        Just $
          T.unlines
            [ gcc <> " -o korrvigs.out " <> T.pack code,
              "chmod +x korrvigs.out"
            ],
      _exeRun = proc "./korrvigs.out" $ T.unpack <$> args
    }

hashRunnable ::
  (MonadFail m) =>
  (Id -> m Hash) ->
  (Id -> Text -> m Hash) ->
  Id ->
  Runnable ->
  m Hash
hashRunnable hashEntry hashComp curId rbl = fmap doHash . execWriterT $ do
  tell $ buildExe $ rbl ^. runExecutable
  tell sep
  tell $ stringUtf8 $ T.unpack $ rbl ^. runCode
  tell sep
  tell $ stringUtf8 $ T.unpack $ runTypeName $ rbl ^. runType
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
    buildExe PlainJson = stringUtf8 "json"
    buildExe PlainCsv = stringUtf8 "csv"
    buildExe PlainText = stringUtf8 "text"
    buildExe Graphviz = stringUtf8 "graphviz"
    buildExe CLang = stringUtf8 "c"
    buildExe CPPLang = stringUtf8 "cpp"
    buildRunArg (ArgPlain txt) = do
      tell $ char8 'p'
      tell $ stringUtf8 $ T.unpack txt
    buildRunArg (ArgResult i cmp) = do
      hash <- lift $ hashComp i cmp
      tell $ char8 'c'
      tell $ stringUtf8 $ T.unpack $ digestToHexa hash
    buildRunArg (ArgResultSame cmp) = buildRunArg (ArgResult curId cmp)
    buildRunArg (ArgEntry i) = do
      hash <- lift $ hashEntry i
      tell $ char8 'e'
      tell $ stringUtf8 $ T.unpack $ digestToHexa hash

run ::
  (MonadUnliftIO m, MonadResource m) =>
  Runnable ->
  (FilePath -> RunArg -> m Text) ->
  ConduitT () ByteString m () -> -- stdin
  ConduitT ByteString Void m a -> -- stdout
  ConduitT ByteString Void m b -> -- stderr
  m (ExitCode, a, b)
run rbl resolveArg stdin stdout stderr = withRunInIO $ \runInIO ->
  withSystemTempDirectory "korrvigs" $ \tmp -> runInIO $ do
    (mCompilePrc, prc) <- liftIO $ runProc (runInIO . resolveArg tmp) tmp rbl
    forM_ mCompilePrc $ \compilePrc ->
      sourceProcessWithStreams compilePrc (sourceFile "/dev/null") sinkNull sinkNull
    sourceProcessWithStreams prc stdin stdout stderr

runInOut ::
  (MonadUnliftIO m, MonadResource m) =>
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
  ConduitT ByteString Void m b -> -- stderr
  m (ExitCode, a, b)
runOut rbl resolveArg stdout stderr = withRunInIO $ \runInIO -> do
  withSystemTempDirectory "korrvigs" $ \tmp -> runInIO $ do
    (mCompilePrc, prc) <- liftIO $ runProc (runInIO . resolveArg tmp) tmp rbl
    forM_ mCompilePrc $ \compilePrc ->
      sourceProcessWithStreams compilePrc (sourceFile "/dev/null") sinkNull sinkNull
    case rbl ^. runStdIn of
      Just stdinV -> do
        stdinPath <- resolveArg tmp stdinV
        let stdin = sourceFile $ T.unpack stdinPath
        sourceProcessWithStreams prc stdin stdout stderr
      Nothing -> do
        sourceProcessWithStreams prc (sourceFile "/dev/null") stdout stderr
