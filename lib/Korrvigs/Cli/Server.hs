module Korrvigs.Cli.Server where

import Control.Lens
import Data.Maybe
import Korrvigs.Cli.Monad
import Korrvigs.Monad
import Korrvigs.Utils.Base16
import Korrvigs.Web ()
import Korrvigs.Web.Backend
import Options.Applicative
import Yesod
import Yesod.Static

data Cmd = Cmd {_port :: Maybe Int, _staticPath :: Maybe FilePath}

makeLenses ''Cmd

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
  pwd <- view $ korrWeb . webPassword
  salt <- view $ korrWeb . webSalt
  theme <- view $ korrWeb . webTheme
  let staticP = fromMaybe "./static" $ cmd ^. staticPath
  stc <- liftIO $ staticDevel staticP
  liftIO $
    warp prt $
      WebData
        { web_connection = conn,
          web_root = rt,
          web_theme = theme16 theme,
          web_pwd = pwd,
          web_salt = salt,
          web_static = stc
        }
