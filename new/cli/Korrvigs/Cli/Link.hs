module Korrvigs.Cli.Link where

import Control.Lens hiding (argument)
import Data.Text (Text)
import Korrvigs.Cli.Monad
import Options.Applicative

data Cmd = New {_newUrl :: Text, _newOffline :: Bool}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "new"
      ( info
          ( ( New
                <$> argument str (metavar "URL")
                <*> switch (long "offline" <> help "Do not download information about URL")
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
run (New _ _) = undefined
