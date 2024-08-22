module Korrvigs.Cli.Init where

import Control.Lens hiding (argument)
import Control.Monad
import Data.Aeson
import Data.Aeson.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Korrvigs.Cli.Monad
import Options.Applicative hiding (value)
import System.Directory
import System.FilePath
import System.Process

-- The database must exists with the following extension created:
--   CREATE EXTENSION postgis;
--   CREATE EXTENSION address_standardizer;

data Cmd = Cmd
  { _rootPath :: FilePath,
    _psqlSpec :: Text,
    _skipGitInit :: Bool,
    _skipAnnexInit :: Bool,
    _skipDBInit :: Bool
  }

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  Cmd
    <$> argument str (metavar "DIR" <> help "Root directory")
    <*> argument str (metavar "PSQL" <> help "Connection string for PostgreSQL")
    <*> switch (long "skip-git-init" <> help "Do not init the root directory as a git repository")
    <*> switch (long "skip-annex-init" <> help "Do not init the root directory as a git annex repository")
    <*> switch (long "skip-db-init" <> help "Do not create the tables")

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Init a korrvigs database"
      <> header "korr init -- Init database"

run :: Cmd -> IO ()
run cmd = do
  let rt = cmd ^. rootPath
  let config = KConfig rt (cmd ^. psqlSpec)
  -- Store config
  putStrLn ">>> Saving configuration"
  cfgPath <- configPath
  let cfgDir = takeDirectory cfgPath
  createDirectoryIfMissing True cfgDir
  let cfgContent = encodingToLazyByteString $ value $ toJSON config
  BSL.writeFile cfgPath cfgContent
  -- Create root directory
  putStrLn ">>> Creating root directory"
  createDirectoryIfMissing True rt
  -- git init
  unless (cmd ^. skipGitInit) $ do
    putStrLn ">>> Initing git repository"
    let git = (proc "git" ["init"]) {cwd = Just rt}
    (_, _, _, prc) <- createProcess git
    void $ waitForProcess prc
  -- git annex init
  unless (cmd ^. skipAnnexInit) $ do
    putStrLn ">>> Initing git annex repository"
    let annex = (proc "git" ["annex", "init"]) {cwd = Just rt}
    (_, _, _, prc) <- createProcess annex
    void $ waitForProcess prc
  -- create database schema
  unless (cmd ^. skipDBInit) $ do
    putStrLn ">>> Creating database schema"
    conn <- connectPostgreSQL $ Enc.encodeUtf8 $ cmd ^. psqlSpec
    let schemaSQL = $(embedFile "doc/schema.pgsql")
    let queries = filter (not . BS.null) $ BS8.strip <$> BS.split 59 schemaSQL -- 59 is ASCII for ;
    forM_ queries $ execute_ conn . Query
    close conn
