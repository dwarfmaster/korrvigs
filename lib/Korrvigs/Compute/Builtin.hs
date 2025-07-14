module Korrvigs.Compute.Builtin where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Korrvigs.Compute.Declare
import Korrvigs.Entry
import Korrvigs.Monad.Class
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Process
import Network.Mime (MimeType)
import System.Process
import Text.Parsec hiding (unexpected)
import Text.Parsec.Number

data Action
  = Miniature
  | Size
  deriving (Eq, Show)

makePrisms ''Action

instance ToJSON Action where
  toJSON Miniature = String "miniature"
  toJSON Size = String "size"

instance FromJSON Action where
  parseJSON (String "miniature") = pure Miniature
  parseJSON (String "size") = pure Size
  parseJSON v = unexpected v

isPrefix :: Text -> MimeType -> Bool
isPrefix prefix mime = T.isPrefixOf prefix $ Enc.decodeASCII mime

run :: (MonadKorrvigs m) => Action -> Entry -> FilePath -> m ()
run Miniature entry tgt = case entry ^. kindData of
  FileD file | file ^. fileStatus == FileAbsent -> pure ()
  FileD file
    | isPrefix "image/" (file ^. fileMime) ->
        let magick = proc "magick" [file ^. filePath <> "[0]", "-resize", "200x200", tgt]
         in liftIO $ void $ runSilent magick
  FileD file
    | isPrefix "video/" (file ^. fileMime) ->
        let ffmpeg = proc "ffmpeg" ["-i", "file:" <> file ^. filePath, "-vframes", "1", "-f", "image2", "-vf", "scale=200:-2", tgt]
         in liftIO $ void $ runSilent ffmpeg
  _ -> pure ()
run Size entry tgt = case entry ^. kindData of
  FileD file | file ^. fileStatus == FileAbsent -> pure ()
  FileD file
    | isPrefix "image/" (file ^. fileMime) -> do
        let magick = proc "magick" ["identify", "-auto-orient", "-format", "%w %h\n", file ^. filePath]
        (_, content) <- liftIO $ runStdout magick
        let parser = sepEndBy ((,) <$> decimal <*> (char ' ' *> decimal)) (void newline)
        case runParser parser () "" content of
          Right (dimensions :: [(Int, Int)]) ->
            let w = maximum $ fst <$> dimensions
             in let h = maximum $ snd <$> dimensions
                 in writeJsonToFile tgt $ object ["width" .= toJSON w, "height" .= toJSON h]
          Left _ -> pure ()
  FileD file | isPrefix "video/" (file ^. fileMime) -> do
    let ffprobe = proc "ffprobe" [file ^. filePath, "-v", "error", "-select_streams", "v:0", "-show_entries", "stream=width,height", "-of", "csv=s=x:p=0"]
    (_, content) <- liftIO $ runStdout ffprobe
    let parser = sepEndBy decimal (char 'x')
    case runParser parser () "" content of
      Right ([w, h] :: [Int]) ->
        writeJsonToFile tgt $ object ["width" .= toJSON w, "height" .= toJSON h]
      _ -> pure ()
  _ -> pure ()

actionData :: Action -> ActionData
actionData Miniature = ActionData Picture def Nothing
actionData Size = ActionData Json def Nothing
