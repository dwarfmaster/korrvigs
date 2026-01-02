{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Korrvigs.Cli.Monad where

import Conduit (MonadThrow, MonadUnliftIO, throwM)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Korrvigs.Monad
import Korrvigs.Utils (lazyCreateManager)
import Korrvigs.Utils.Base16
import Korrvigs.Utils.JSON (fromJSONM)
import Network.HTTP.Client hiding (path)
import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath

data WebState = WState
  { _webPort :: Int,
    _webTheme :: Base16Data,
    _webStaticDir :: Maybe FilePath,
    _webStaticRedirect :: Maybe Text
  }

data KorrState = KState
  { _korrConnection :: Connection,
    _korrSQLLock :: MVar (),
    _korrRoot :: FilePath,
    _korrCaptureRoot :: FilePath,
    _korrMimeDatabase :: FilePath,
    _korrWeb :: WebState,
    _korrCreds :: Map Text Value,
    _korrTokens :: TVar (Map Text Value),
    _korrManager :: IORef (Maybe Manager)
  }

data KorrConfig = KConfig
  { _kconfigRoot :: FilePath,
    _kconfigPsql :: Text,
    _kconfigPort :: Int,
    _kconfigTheme :: Base16Data,
    _kconfigStaticDir :: Maybe FilePath,
    _kconfigStaticRedirect :: Maybe Text,
    _kconfigCaptureRoot :: FilePath,
    _kconfigCredentials :: Maybe FilePath,
    _kconfigMimeDatabase :: FilePath
  }

makeLenses ''KorrState
makeLenses ''WebState
makeLenses ''KorrConfig

newtype KorrM a = KorrM (ReaderT KorrState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader KorrState, MonadThrow, MonadUnliftIO)

instance MonadKorrvigs KorrM where
  withSQL act = do
    lock <- view korrSQLLock
    liftIO $ takeMVar lock
    conn <- view korrConnection
    r <- act conn
    liftIO $ putMVar lock ()
    pure r
  root = view korrRoot
  captureRoot = view korrCaptureRoot
  mimeDatabase = view korrMimeDatabase
  getCredential c = do
    creds <- view korrCreds
    pure $ M.lookup c creds >>= fromJSONM
  manager = view korrManager >>= liftIO . lazyCreateManager
  getToken tok = do
    tv <- view korrTokens
    liftIO $ atomically $ do
      toks <- readTVar tv
      pure $ M.lookup tok toks >>= fromJSONM
  storeToken tok v = do
    tv <- view korrTokens
    liftIO $ atomically $ modifyTVar tv $ M.insert tok $ toJSON v

instance MonadFail KorrM where
  fail s = throwM $ KMiscError $ T.pack s

runKorrM :: KorrConfig -> KorrM a -> IO (Either KorrvigsError a)
runKorrM config act = do
  conn <- connectPostgreSQL $ Enc.encodeUtf8 $ config ^. kconfigPsql
  creds <- case config ^. kconfigCredentials of
    Nothing -> pure M.empty
    Just credsPath -> fromMaybe M.empty . decode <$> BSL.readFile credsPath
  ref <- newIORef Nothing
  toks <- newTVarIO M.empty
  lock <- newMVar ()
  let state =
        KState
          { _korrConnection = conn,
            _korrSQLLock = lock,
            _korrRoot = config ^. kconfigRoot,
            _korrCaptureRoot = config ^. kconfigCaptureRoot,
            _korrMimeDatabase = config ^. kconfigMimeDatabase,
            _korrWeb =
              WState
                { _webPort = config ^. kconfigPort,
                  _webTheme = config ^. kconfigTheme,
                  _webStaticDir = config ^. kconfigStaticDir,
                  _webStaticRedirect = config ^. kconfigStaticRedirect
                },
            _korrCreds = creds,
            _korrTokens = toks,
            _korrManager = ref
          }
  let (KorrM action) = setupPsql >> act
  r <- catch (Right <$> runReaderT action state) (pure . Left)
  close conn
  case r of
    Left some ->
      case fromException some of
        Just e -> pure $ Left e
        Nothing -> throwM some
    Right v -> pure $ Right v

instance FromJSON KorrConfig where
  parseJSON =
    withObject "Config" $ \v ->
      KConfig
        <$> v .: "root"
        <*> v .: "connectionSpec"
        <*> v .: "port"
        <*> parseJSON (Object v)
        <*> v .:? "staticDir"
        <*> v .:? "staticRedirect"
        <*> v .: "capture"
        <*> v .:? "credentials"
        <*> v .: "mimeDatabase"

configPath :: IO FilePath
configPath = do
  cfgDir <- getUserConfigDir "korrvigs"
  let xdgPath = cfgDir </> "config.json"
  home <- getHomeDirectory
  let homePath = home </> "korrvigs.json"
  xdgEx <- doesFileExist xdgPath
  homeEx <- doesFileExist homePath
  pure $ if homeEx && not xdgEx then homePath else xdgPath

loadConfig :: FilePath -> IO (Maybe KorrConfig)
loadConfig path = decode <$> BSL.readFile path

runKorrMWithConfig :: KorrM a -> IO (Either KorrvigsError a)
runKorrMWithConfig action = do
  cfgPath <- configPath
  r <- catch (Right <$> loadConfig cfgPath) $ pure . Left
  case r of
    Left err -> pure $ Left $ KIOError err
    Right Nothing ->
      pure $ Left $ KMiscError $ "Failed to parse config file at " <> T.pack cfgPath
    Right (Just cfg) ->
      runKorrM cfg action
