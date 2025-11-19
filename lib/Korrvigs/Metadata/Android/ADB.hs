module Korrvigs.Metadata.Android.ADB where

import Conduit
import Data.Conduit.Combinators
import Data.Conduit.Process
import Data.Conduit.Text
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Utils.Process
import System.Exit
import System.FilePath

startServer :: IO Bool
startServer = do
  exit <- runSilent $ proc "adb" ["start-server"]
  pure $ exit == ExitSuccess

connectedDevice :: IO (Maybe Text)
connectedDevice = do
  (exit, out) <- runStdout $ proc "adb" ["devices"]
  case exit of
    ExitFailure _ -> pure Nothing
    ExitSuccess ->
      let txt = LEnc.decodeUtf8 out
       in case LT.words <$> LT.lines txt of
            (_ : (i : _) : _) -> pure $ Just $ LT.toStrict i
            _ -> pure Nothing

files :: Text -> IO (Maybe (Set Text))
files dir = do
  let cmd = "find \"" <> T.unpack dir <> "\" -maxdepth 1 -not -type d"
  let prc = proc "adb" ["shell", cmd]
  (exit, out) <- sourceProcessWithConsumer prc consumer
  pure $ case exit of
    ExitFailure _ -> Nothing
    ExitSuccess -> Just $ S.fromList out
  where
    consumer = decode utf8 .| linesUnbounded .| mapC filename .| sinkList
    filename = T.pack . takeFileName . T.unpack

pull :: FilePath -> FilePath -> IO Bool
pull androidPath targetPath = do
  exit <- runSilent $ proc "adb" ["pull", androidPath, targetPath]
  pure $ case exit of
    ExitSuccess -> True
    ExitFailure _ -> False
