{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Korrvigs.Cli.Monad where

import Conduit (MonadThrow, MonadUnliftIO, throwM)
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Korrvigs.Monad
import Korrvigs.Utils.Base16
import Korrvigs.Utils.JSON (fromJSONM)
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
    _korrRoot :: FilePath,
    _korrCalsyncRoot :: FilePath,
    _korrCaptureRoot :: FilePath,
    _korrWeb :: WebState,
    _korrCreds :: Map Text Value
  }

data KorrConfig = KConfig
  { _kconfigRoot :: FilePath,
    _kconfigPsql :: Text,
    _kconfigPort :: Int,
    _kconfigTheme :: Base16Data,
    _kconfigStaticDir :: Maybe FilePath,
    _kconfigStaticRedirect :: Maybe Text,
    _kconfigCalsyncRoot :: FilePath,
    _kconfigCaptureRoot :: FilePath,
    _kconfigCredentials :: Maybe FilePath
  }

makeLenses ''KorrState
makeLenses ''WebState
makeLenses ''KorrConfig

newtype KorrM a = KorrM (ReaderT KorrState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader KorrState, MonadThrow, MonadUnliftIO)

instance MonadKorrvigs KorrM where
  pgSQL = view korrConnection
  root = view korrRoot
  calsyncRoot = view korrCalsyncRoot
  captureRoot = view korrCaptureRoot
  getCredential c = do
    creds <- view korrCreds
    pure $ M.lookup c creds >>= fromJSONM

runKorrM :: KorrConfig -> KorrM a -> IO (Either KorrvigsError a)
runKorrM config act = do
  conn <- connectPostgreSQL $ Enc.encodeUtf8 $ config ^. kconfigPsql
  creds <- case config ^. kconfigCredentials of
    Nothing -> pure M.empty
    Just credsPath -> fromMaybe M.empty . decode <$> BSL.readFile credsPath
  let state =
        KState
          { _korrConnection = conn,
            _korrRoot = config ^. kconfigRoot,
            _korrCalsyncRoot = config ^. kconfigCalsyncRoot,
            _korrCaptureRoot = config ^. kconfigCaptureRoot,
            _korrWeb =
              WState
                { _webPort = config ^. kconfigPort,
                  _webTheme = config ^. kconfigTheme,
                  _webStaticDir = config ^. kconfigStaticDir,
                  _webStaticRedirect = config ^. kconfigStaticRedirect
                },
            _korrCreds = creds
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
        <*> v .: "calsync"
        <*> v .: "capture"
        <*> v .:? "credentials"

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
