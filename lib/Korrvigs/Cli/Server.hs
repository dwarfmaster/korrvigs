module Korrvigs.Cli.Server where

import Control.Lens
import Data.Maybe
import Korrvigs.Cli.Monad
import Korrvigs.Monad
import Korrvigs.Utils.Base16
import Korrvigs.Web ()
import Korrvigs.Web.Backend
import Korrvigs.Web.Public.Crypto
import Options.Applicative
import System.Environment
import Yesod
import Yesod.Static

data Cmd = Cmd {_port :: Maybe Int, _staticPath :: Maybe FilePath}

makeLenses ''Cmd

firstJust :: a -> [Maybe a] -> a
firstJust df [] = df
firstJust _ (Just x : _) = x
firstJust df (Nothing : xs) = firstJust df xs

parser' :: Parser Cmd
parser' =
  Cmd
    <$> optional (option auto (metavar "PORT" <> long "port" <> help "The port to run the server on"))
    <*> optional (option auto (metavar "PATH" <> long "static" <> help "The directory of static js and css files"))

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Serve the korrvigs web interface"
      <> header "korr server -- Serve the web interface"

run :: Cmd -> KorrM ()
run cmd = do
  rt <- root
  conn <- pgSQL
  defaultPort <- view $ korrWeb . webPort
  let prt = fromMaybe defaultPort $ cmd ^. port
  theme <- view $ korrWeb . webTheme
  staticEnv <- liftIO $ lookupEnv "KORRVIGS_WEB_STATIC"
  staticCfg <- view $ korrWeb . webStaticDir
  let staticP = firstJust "./static" [cmd ^. staticPath, staticEnv, staticCfg]
  stc <- liftIO $ staticDevel staticP
  staticRedirect <- view $ korrWeb . webStaticRedirect
  secret <- liftIO loadOrGenerateKey
  liftIO $
    warp prt $
      WebData
        { web_connection = conn,
          web_root = rt,
          web_theme = theme16 theme,
          web_static = stc,
          web_static_redirect = staticRedirect,
          web_mac_secret = secret
        }
