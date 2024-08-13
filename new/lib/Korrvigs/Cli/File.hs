module Korrvigs.Cli.File where

import Control.Lens hiding (argument)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.File
import Options.Applicative

data Cmd = New {_file :: FilePath}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "new"
      ( info
          ( ( New
                <$> argument str (metavar "PATH")
            )
              <**> helper
          )
          ( progDesc "Insert a new file"
              <> header "korr file new -- insert file"
          )
      )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with file entries in Korrvigs"
      <> header "korr file -- interface for files"

run :: Cmd -> KorrM ()
run (New path) = do
  i <- new path Nothing
  liftIO $ putStrLn $ "Inserted \"" <> path <> "\" as " <> T.unpack (unId i)
