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
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Korrvigs.Cli.Monad
import Korrvigs.Utils.Base16
import Options.Applicative hiding (value)
import qualified Options.Applicative as Opt
import System.Directory
import System.FilePath
import System.IO
import System.Process

-- The database must exists with the following extension created:
--   CREATE EXTENSION postgis;
--   CREATE EXTENSION address_standardizer;

data Cmd = Cmd
  { _rootPath :: FilePath,
    _psqlSpec :: Text,
    _port :: Int,
    _theme :: Maybe FilePath,
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
    <*> option auto (long "port" <> metavar "PORT" <> help "Port to serve the web interface on" <> Opt.value 3000)
    <*> optional (option str (long "theme" <> metavar "PATH" <> help "Path to a base16 yaml file"))
    <*> switch (long "skip-git-init" <> help "Do not init the root directory as a git repository")
    <*> switch (long "skip-annex-init" <> help "Do not init the root directory as a git annex repository")
    <*> switch (long "skip-db-init" <> help "Do not create the tables")

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Init a korrvigs database"
      <> header "korr init -- Init database"

withEcho :: Bool -> IO a -> IO a
withEcho echo act = do
  old <- hGetEcho stdin
  hSetEcho stdin echo
  r <- act
  hSetEcho stdin old
  pure r

getPassword :: IO Text
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass1 <- withEcho False getLine
  putChar '\n'
  putStr "Confirm password: "
  hFlush stdout
  pass2 <- withEcho False getLine
  putChar '\n'
  if pass1 == pass2
    then pure $ T.pack pass1
    else putStrLn "Passwords do not match" >> getPassword

defaultTheme :: Base16Data
defaultTheme = B16Data $ \case
  Base00 -> "#231e18"
  Base01 -> "#302b25"
  Base02 -> "#48413a"
  Base03 -> "#9d8b70"
  Base04 -> "#b4a490"
  Base05 -> "#cabcb1"
  Base06 -> "#d7c8bc"
  Base07 -> "#e4d4c8"
  Base08 -> "#d35c5c"
  Base09 -> "#ca7f32"
  Base0A -> "#e0ac16"
  Base0B -> "#b7ba53"
  Base0C -> "#6eb958"
  Base0D -> "#88a4d3"
  Base0E -> "#bb90e2"
  Base0F -> "#b49368"

run :: Cmd -> IO ()
run cmd = do
  let rt = cmd ^. rootPath
  -- Read Base16 theme
  thm <- case cmd ^. theme of
    Nothing -> pure defaultTheme
    Just path -> readB16FromYaml path
  -- The config
  let config =
        KConfig
          { _kconfigRoot = rt,
            _kconfigPsql = cmd ^. psqlSpec,
            _kconfigPort = cmd ^. port,
            _kconfigTheme = thm,
            _kconfigStaticDir = Nothing,
            _kconfigStaticRedirect = Nothing
          }
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
