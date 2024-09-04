{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Korrvigs.Cli.Monad where

import Conduit (MonadThrow, throwM)
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import qualified Korrvigs.Actions as Actions
import Korrvigs.Monad
import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath

data KorrState = KState
  { _korrConnection :: Connection,
    _korrRoot :: FilePath
  }

makeLenses ''KorrState

newtype KorrM a = KorrM (ExceptT SomeException (ReaderT KorrState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader KorrState, MonadThrow)

instance MonadKorrvigs KorrM where
  pgSQL = view korrConnection
  root = view korrRoot
  load = Actions.load
  remove = Actions.remove
  dispatchRemove = Actions.dispatchRemove
  removeDB = Actions.removeDB
  dispatchRemoveDB = Actions.dispatchRemoveDB
  sync = Actions.sync

runKorrM :: ByteString -> FilePath -> KorrM a -> IO (Either KorrvigsError a)
runKorrM connSpec rt (KorrM action) = do
  conn <- connectPostgreSQL connSpec
  r <- runReaderT (runExceptT action) $ KState conn rt
  close conn
  case r of
    Left some ->
      case fromException some of
        Just e -> pure $ Left e
        Nothing -> throwM some
    Right v -> pure $ Right v

data KorrConfig = KConfig
  { _kconfigRoot :: FilePath,
    _kconfigPsql :: Text
  }

makeLenses ''KorrConfig

instance FromJSON KorrConfig where
  parseJSON (Object v) =
    KConfig
      <$> v .: "root"
      <*> v .: "connectionSpec"
  parseJSON invalid =
    prependFailure "parsing Korrvigs config failed, " $ typeMismatch "Object" invalid

instance ToJSON KorrConfig where
  toJSON cfg =
    object
      [ "root" .= (cfg ^. kconfigRoot),
        "connectionSpec" .= (cfg ^. kconfigPsql)
      ]

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
      runKorrM (Enc.encodeUtf8 $ cfg ^. kconfigPsql) (cfg ^. kconfigRoot) action
