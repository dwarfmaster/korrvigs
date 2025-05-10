module Korrvigs.Cli.ADB where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Text as T
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Metadata.Android
import Options.Applicative

data Cmd = Import

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
