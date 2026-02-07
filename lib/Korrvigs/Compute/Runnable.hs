module Korrvigs.Compute.Runnable
  ( Executable (..),
    RunProfile (..),
    RunnableSetup (..),
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
    runSetup,
    runStpProfile,
    runStpFlags,
    runStpVersion,
    runStpOpt,
    runDeps,
    run,
    runInOut,
    runPipe,
    runOut,
  )
where

import Conduit
import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Lens
import Control.Monad
import Control.Monad.Writer.Lazy
import qualified Crypto.Hash as Hsh
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Conduit.Process
import Data.Default
import Data.Foldable (toList)
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Korrvigs.Compute.Type
import Korrvigs.Entry
import Korrvigs.Utils.Crypto
import NeatInterpolation
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
  | NixData
  | Python
  | Lua
  | Julia
  | Dhall
  | Perl
  | Raku
  | Haskell
  | Rust
  | OCaml
  | OpenScad
  | Povray
  | LaTeX
  | ConTeXt
  | GnuPlot
  deriving (Show, Eq, Ord, Bounded, Enum)

data RunArg
  = ArgPlain Text
  | ArgResult Id Text
  | ArgResultSame Text
  | ArgEntry Id
  deriving (Show, Eq, Ord)

data RunProfile
  = RunRelease
  | RunDebug
  deriving (Show, Eq, Ord)

data RunnableSetup = RunnableSetup
  { _runStpProfile :: Maybe RunProfile,
    _runStpOpt :: Maybe Text,
    _runStpFlags :: [Text],
    _runStpVersion :: Maybe Text
  }
  deriving (Show, Eq, Ord)

data Runnable = Runnable
  { _runExecutable :: Executable,
    _runCode :: Text,
    _runType :: RunnableType,
    _runArgs :: [RunArg],
    _runEnv :: Map Text RunArg,
    _runStdIn :: Maybe RunArg,
    _runSetup :: RunnableSetup
  }
  deriving (Show, Eq, Ord)

makeLenses ''RunnableSetup
makeLenses ''Runnable
makePrisms ''RunArg

instance Default RunnableSetup where
  def = RunnableSetup Nothing Nothing [] Nothing

instance Semigroup RunnableSetup where
  r1 <> r2 =
    RunnableSetup
      { _runStpProfile = r1 ^. runStpProfile <|> r2 ^. runStpProfile,
        _runStpOpt = r1 ^. runStpOpt <|> r2 ^. runStpOpt,
        _runStpFlags = r1 ^. runStpFlags <> r2 ^. runStpFlags,
        _runStpVersion = r1 ^. runStpVersion <|> r2 ^. runStpVersion
      }

instance Monoid RunnableSetup where
  mempty = def

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
  let exePrc = mkExeProc (rbl ^. runExecutable) (rbl ^. runSetup) args (rbl ^. runType)
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

procArgs :: Text -> RunnableSetup -> [Text] -> CreateProcess
procArgs bin stp = proc (T.unpack bin) . map T.unpack . (stp ^. runStpFlags <>)

onDebug :: [Text] -> RunProfile -> [Text]
onDebug flags RunDebug = flags
onDebug _ RunRelease = []

mkExeProc :: Executable -> RunnableSetup -> [Text] -> RunnableType -> ExeProc
mkExeProc Bash stp args _ =
  noCompile "code.sh" $ procArgs "bash" stp $ "code.sh" : args
mkExeProc SwiProlog stp args _ =
  noCompile "code.pl" $ procArgs "swipl" stp $ "code.pl" : "--" : args
mkExeProc PlainJson _ _ _ =
  noCompile "data.json" $ proc "cat" ["data.json"]
mkExeProc PlainCsv _ _ _ =
  noCompile "data.csv" $ proc "cat" ["data.csv"]
mkExeProc PlainText _ _ _ =
  noCompile "data.txt" $ proc "cat" ["data.txt"]
mkExeProc Graphviz stp _ tp = noCompile "data.dot" $ case tp of
  ScalarImage -> procArgs "dot" stp ["-Tjpg", "data.dot"]
  ScalarGraphic -> procArgs "dot" stp ["-Tpng", "data.dot"]
  VectorGraphic -> procArgs "dot" stp ["-Tsvg", "data.dot"]
  _ -> proc "false" []
mkExeProc CLang stp' args _ = mkCLikeBuildScript cfg "gcc" stp "code.c" args
  where
    cfg = (onDebug ["-g"], singleton . ("-O" <>), singleton . ("-std=" <>))
    stp = stp' & runStpVersion %~ Just . fromMaybe "c23"
mkExeProc CPPLang stp' args _ = mkCLikeBuildScript cfg "g++" stp "code.cpp" args
  where
    cfg = (onDebug ["-g"], singleton . ("-O" <>), singleton . ("-std=" <>))
    stp = stp' & runStpVersion %~ Just . fromMaybe "c++23"
mkExeProc NixData stp _ _ =
  noCompile "data.nix" $ procArgs "nix" stp ["eval", "--impure", "--json", "--file", "data.nix"]
mkExeProc Python stp args _ =
  noCompile "code.py" $ procArgs "python3" stp $ "code.py" : args
mkExeProc Lua stp args _ =
  noCompile "code.lua" $ procArgs "lua" stp $ "code.lua" : args
mkExeProc Julia stp args _ =
  noCompile "code.jl" $ procArgs "julia" stp $ "code.jl" : args
mkExeProc Dhall stp _ _ =
  noCompile "data.dhall" $ procArgs "dhall-to-json" stp ["--file", "data.dhall"]
mkExeProc Raku stp args _ =
  noCompile "code.raku" $ procArgs "raku" stp $ "code.raku" : args
mkExeProc Perl stp args _ =
  noCompile "code.pl" $ procArgs "perl" stp $ "code.pl" : args
mkExeProc Haskell stp' args _ = mkCLikeBuildScript cfg "ghc" stp "code.hs" args
  where
    cfg = (onDebug ["-g"], singleton . ("-O" <>), singleton . ("-X" <>))
    stp = stp' & runStpVersion %~ Just . fromMaybe "Haskell2010"
mkExeProc Rust stp args _ = mkCLikeBuildScript cfg "rustc" stp "code.rs" args
  where
    cfg = (onDebug ["-C", "debuginfo=2"], \o -> ["-C", "opt-level=" <> o], const [])
mkExeProc OCaml stp args _ = mkCLikeBuildScript cfg "ocamlc" stp "code.ml" args
  where
    cfg = (onDebug ["-g"], const [], const [])
mkExeProc OpenScad stp _ VectorGraphic =
  noCompile "model.scad" $ procArgs "openscad" stp ["model.scad", "--export-format", "svg", "-o", "-"]
mkExeProc OpenScad stp _ Model3D = openScad3d stp
mkExeProc OpenScad stp _ _ =
  noCompile "model.scad" $ procArgs "openscad" stp ["model.scad", "--export-format", "png", "-o", "-"]
mkExeProc Povray stp _ _ =
  noCompile "model.pov" $ procArgs "povray" stp ["model.pov", "-o-"]
mkExeProc LaTeX stp _ _ = texBuild stp "pdflatex"
mkExeProc ConTeXt stp _ _ = texBuild stp "context"
mkExeProc GnuPlot stp _ tp =
  noCompile "code.dem" $ procArgs "gnuplot" stp ["-e", "set term " <> tpFlag tp, "code.dem"]
  where
    tpFlag ScalarImage = "jpeg"
    tpFlag ScalarGraphic = "png"
    tpFlag VectorGraphic = "svg"
    tpFlag VectorDocument = "pdf"
    tpFlag _ = "jpeg"

bashFlags :: [Text] -> Text
bashFlags flags = T.intercalate " " $ escape <$> flags
  where
    escape f = "'" <> T.replace "'" "\\'" f <> "'"

mkCLikeBuildScript ::
  (RunProfile -> [Text], Text -> [Text], Text -> [Text]) ->
  Text ->
  RunnableSetup ->
  FilePath ->
  [Text] ->
  ExeProc
mkCLikeBuildScript (prof, opt, version) gcc stp code args =
  ExeProc
    { _exeCode = code,
      _exeCompileScript =
        Just $ gcc <> " " <> bashFlags flags <> " -o korrvigs.out " <> T.pack code,
      _exeRun = proc "./korrvigs.out" $ T.unpack <$> args
    }
  where
    mkFlag :: (a -> [Text]) -> Maybe a -> [Text]
    mkFlag bld val = bld =<< toList val
    flags =
      mkFlag prof (stp ^. runStpProfile)
        <> mkFlag opt (stp ^. runStpOpt)
        <> mkFlag version (stp ^. runStpVersion)
        <> stp ^. runStpFlags

openScad3d :: RunnableSetup -> ExeProc
openScad3d stp =
  ExeProc
    { _exeCode = "model.scad",
      _exeCompileScript =
        Just
          [trimming|
        openscad $flags model.scad --export-format stl -o model.stl
        assimp export model.stl model.glb
      |],
      _exeRun = proc "cat" ["model.glb"]
    }
  where
    flags = bashFlags $ stp ^. runStpFlags

texBuild :: RunnableSetup -> Text -> ExeProc
texBuild stp tex =
  ExeProc
    { _exeCode = "doc.tex",
      _exeCompileScript = Just $ tex <> " " <> flags <> " doc.tex",
      _exeRun = proc "cat" ["doc.pdf"]
    }
  where
    flags = bashFlags $ stp ^. runStpFlags

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
  forM_ (rbl ^. runSetup . runStpProfile) $ \case
    RunRelease -> tell "r"
    RunDebug -> tell "d"
  tell sep
  forM_ (rbl ^. runSetup . runStpOpt) $ tell . stringUtf8 . T.unpack
  tell sep
  tell $ stringUtf8 $ show $ length $ rbl ^. runSetup . runStpFlags
  tell sep
  forM_ (rbl ^. runSetup . runStpFlags) $ \flag -> do
    tell $ stringUtf8 $ T.unpack flag
    tell sep
  forM_ (rbl ^. runSetup . runStpVersion) $ tell . stringUtf8 . T.unpack
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
    buildExe NixData = stringUtf8 "nix"
    buildExe Python = stringUtf8 "python"
    buildExe Lua = stringUtf8 "lua"
    buildExe Julia = stringUtf8 "julia"
    buildExe Dhall = stringUtf8 "dhall"
    buildExe Raku = stringUtf8 "raku"
    buildExe Perl = stringUtf8 "perl"
    buildExe Haskell = stringUtf8 "haskell"
    buildExe Rust = stringUtf8 "rust"
    buildExe OCaml = stringUtf8 "ocaml"
    buildExe OpenScad = stringUtf8 "openscad"
    buildExe Povray = stringUtf8 "povray"
    buildExe LaTeX = stringUtf8 "latex"
    buildExe ConTeXt = stringUtf8 "context"
    buildExe GnuPlot = stringUtf8 "gnuplot"
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

runImpl ::
  (MonadUnliftIO m, MonadResource m) =>
  Runnable ->
  FilePath ->
  (FilePath -> RunArg -> m Text) ->
  ConduitT () ByteString m () -> -- stdin
  ConduitT ByteString Void m a -> -- stdout
  ConduitT ByteString Void m b -> -- stderr
  m (ExitCode, a, b)
runImpl rbl tmp resolveArg stdin stdout stderr = do
  (mCompilePrc, prc) <- runProc (resolveArg tmp) tmp rbl
  compResult <- forM mCompilePrc $ \compilePrc ->
    sourceProcessWithStreams compilePrc (sourceFile "/dev/null") stdout stderr
  case compResult of
    Just (ExitFailure e, a, b) -> pure (ExitFailure e, a, b)
    _ -> sourceProcessWithStreams prc stdin stdout stderr

run ::
  (MonadUnliftIO m, MonadResource m) =>
  Runnable ->
  (FilePath -> RunArg -> m Text) ->
  ConduitT () ByteString m () -> -- stdin
  ConduitT ByteString Void m a -> -- stdout
  ConduitT ByteString Void m b -> -- stderr
  m (ExitCode, a, b)
run rbl resolveArg stdin stdout stderr = withRunInIO $ \runInIO ->
  withSystemTempDirectory "korrvigs" $ \tmp ->
    runInIO $ runImpl rbl tmp resolveArg stdin stdout stderr

runPipe ::
  (MonadUnliftIO m, MonadResource m) =>
  NonEmpty (Id, Runnable) ->
  (Id -> FilePath -> RunArg -> m Text) ->
  ConduitT () ByteString m () -> -- stdin
  ConduitT ByteString Void m a -> -- stdout
  m (Either ExitCode a) -- Return the exit code of the first failed runnable or the final value
runPipe (rbl :| []) resolveArg stdin stdout = do
  (exit, r) <- runInOut (rbl ^. _2) (resolveArg $ rbl ^. _1) stdin stdout
  case exit of
    ExitSuccess -> pure $ Right r
    ExitFailure _ -> pure $ Left exit
runPipe (rbl :| (nrbl : nrbls)) resolveArg stdin stdout = do
  (exit, output) <- runInOut (rbl ^. _2) (resolveArg $ rbl ^. _1) stdin sinkLazy
  case exit of
    ExitFailure _ -> pure $ Left exit
    ExitSuccess -> runPipe (nrbl :| nrbls) resolveArg (sourceLazy output) stdout

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
    let stdin = case rbl ^. runStdIn of
          Just stdinV -> do
            stdinPath <- lift $ resolveArg tmp stdinV
            sourceFile $ T.unpack stdinPath
          Nothing -> sourceFile "/dev/null"
    runImpl rbl tmp resolveArg stdin stdout stderr
