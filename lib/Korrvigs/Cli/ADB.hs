module Korrvigs.Cli.ADB where

import Control.Lens hiding (argument, ignored)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import Korrvigs.Actions
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Android
import Options.Applicative
import System.Directory
import System.FilePath

data Cmd
  = Import
  | Ignore {_toIgnore :: [FilePath]}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "import"
      ( info
          (pure Import <**> helper)
          ( progDesc "Import files from connected phone"
              <> header "korr adb import -- import files from phone"
          )
      )
      <> command
        "ignore"
        ( info
            ( (Ignore <$> many (argument str $ metavar "FILE" <> help "File to ignore"))
                <**> helper
            )
            ( progDesc "Mark file as ignored when importing from android"
                <> header "korr adb ignore -- mark file as ignored"
            )
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with android devices using adb"
      <> header "korr adb -- Deal with android devices"

run :: Cmd -> KorrM ()
run Import =
  importAndroidFiles
    >>= liftIO . putStrLn . \case
      Just phone -> "Imported from @" <> T.unpack (unId $ phone ^. androidEntry)
      Nothing -> "Failed to import"
run (Ignore files) =
  forM_ files $ \file -> do
    path <- liftIO $ canonicalizePath file
    phones <- listPhones
    recogniseCaptured path >>= \case
      Nothing -> liftIO $ putStrLn $ "\"" <> path <> "\" is not an imported file"
      Just (adb, rel) -> case M.lookup adb phones of
        Nothing -> liftIO $ putStrLn $ "\"" <> path <> "\" does not belong to any known phone"
        Just phone ->
          load (phone ^. androidEntry) >>= \case
            Nothing -> liftIO $ putStrLn $ "Failed to load @" <> T.unpack (unId $ phone ^. androidEntry)
            Just phoneEntry -> do
              ignored <- rSelectListMtdt AndroidIgnored $ sqlId $ phone ^. androidEntry
              let newIgnored = T.pack (takeFileName rel) : ignored
              updateMetadata phoneEntry (M.singleton (mtdtSqlName AndroidIgnored) $ toJSON newIgnored) []
              liftIO $ removeFile path
