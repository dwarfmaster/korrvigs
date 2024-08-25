module Korrvigs.Cli.Link where

import Control.Lens hiding (argument)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Link.New
import Korrvigs.Utils.DateParser (dayParser)
import Options.Applicative

data Cmd = New {_newUrl :: Text, _newOptions :: NewLink}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "new"
      ( info
          ( ( New
                <$> argument str (metavar "URL")
                <*> ( NewLink
                        <$> switch (long "offline" <> help "Do not download information about URL")
                        <*> optional (option dayParser $ metavar "DATE" <> long "data")
                        <*> optional (option str $ metavar "TITLE" <> long "title")
                        <*> pure Nothing
                    )
            )
              <**> helper
          )
          ( progDesc "Add a new URL if it is not already present, and print its ID"
              <> header "korr link new -- insert new link"
          )
      )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with link entries in Korrvigs"
      <> header "korr link -- interface for links"

run :: Cmd -> KorrM ()
run (New url options) = do
  i <- new url options
  liftIO $ putStrLn $ T.unpack $ unId i
