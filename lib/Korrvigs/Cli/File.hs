module Korrvigs.Cli.File where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New
import Korrvigs.Entry
import Korrvigs.File
import Korrvigs.File.Mtdt
import Korrvigs.Monad
import Options.Applicative

data Cmd
  = New {_nfFile :: FilePath, _nfOptions :: NewFile}
  | ReExtract {_reFiles :: [Id]}

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
                        <*> switch (long "delete" <> help "Delete original file after insertion")
                    )
            )
              <**> helper
          )
          ( progDesc "Insert a new file"
              <> header "korr file new -- insert file"
          )
      )
      <> command
        "re-extract"
        ( info
            ((ReExtract <$> many (MkId <$> argument str (metavar "ID"))) <**> helper)
            ( progDesc "Re-extract metadata"
                <> header "korr file re-extract -- re-extract metadaa"
            )
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with file entries in Korrvigs"
      <> header "korr file -- interface for files"

run :: Cmd -> KorrM ()
run (New path options) = do
  i <- new path options
  liftIO $ putStrLn $ T.unpack $ unId i
run (ReExtract fileIDs) = forM_ fileIDs $ \i -> do
  entry <- load i >>= throwMaybe (KCantLoad i "Failed to load entry to re-extract")
  file <- throwMaybe (KMiscError $ unId i <> " is not a file") $ entry ^? _File
  reextractMetadata file
