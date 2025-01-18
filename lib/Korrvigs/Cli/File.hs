module Korrvigs.Cli.File where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New
import Korrvigs.Entry
import Korrvigs.File
import Options.Applicative
import System.Directory (removeFile)

data Cmd = New {_nfFile :: FilePath, _nfOptions :: NewFile, _nfRemove :: Bool}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "new"
      ( info
          ( ( New
                <$> argument str (metavar "PATH")
                <*> ( NewFile
                        <$> newEntryOptions
                    )
                <*> switch (long "delete" <> help "Delete original file after insertion")
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
run (New path options remove) = do
  i <- new path options
  when remove $ liftIO $ removeFile path
  liftIO $ putStrLn $ T.unpack $ unId i
