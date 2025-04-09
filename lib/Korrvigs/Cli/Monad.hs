{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Korrvigs.Cli.Monad where

import Conduit (MonadThrow, MonadUnliftIO, throwM)
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Korrvigs.Monad
import Korrvigs.Utils.Base16
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
    _korrWeb :: WebState
  }

data KorrConfig = KConfig
  { _kconfigRoot :: FilePath,
    _kconfigPsql :: Text,
    _kconfigPort :: Int,
    _kconfigTheme :: Base16Data,
    _kconfigStaticDir :: Maybe FilePath,
    _kconfigStaticRedirect :: Maybe Text
  }

makeLenses ''KorrState
makeLenses ''WebState
makeLenses ''KorrConfig

newtype KorrM a = KorrM (ReaderT KorrState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader KorrState, MonadThrow, MonadUnliftIO)

instance MonadKorrvigs KorrM where
  pgSQL = view korrConnection
  root = view korrRoot

runKorrM :: KorrConfig -> KorrM a -> IO (Either KorrvigsError a)
runKorrM config act = do
  conn <- connectPostgreSQL $ Enc.encodeUtf8 $ config ^. kconfigPsql
  let state =
        KState
          { _korrConnection = conn,
            _korrRoot = config ^. kconfigRoot,
            _korrWeb =
              WState
                { _webPort = config ^. kconfigPort,
                  _webTheme = config ^. kconfigTheme,
                  _webStaticDir = config ^. kconfigStaticDir,
                  _webStaticRedirect = config ^. kconfigStaticRedirect
                }
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

instance ToJSON KorrConfig where
  toJSON cfg =
    object $
      [ "root" .= (cfg ^. kconfigRoot),
        "connectionSpec" .= (cfg ^. kconfigPsql),
        "port" .= (cfg ^. kconfigPort)
      ]
        ++ fmap (\b -> K.fromText (baseName b) .= theme16 (cfg ^. kconfigTheme) b) [minBound .. maxBound]

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
