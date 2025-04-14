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
import Korrvigs.Monad
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
        let magick = proc "magick" [file ^. filePath, "-resize", "200x200", tgt]
         in liftIO $ void $ runSilent magick
  FileD file
    | isPrefix "video/" (file ^. fileMime) ->
        let ffmpeg = proc "ffmpeg" ["-i", "file:" <> file ^. filePath, "-vframes", "1", "-f", "image2", "-vf", "scale=200:-2", tgt]
         in liftIO $ void $ runSilent ffmpeg
  _ -> pure ()
run Size entry tgt = case entry ^. kindData of
  FileD file | file ^. fileStatus /= FilePresent -> pure ()
  FileD file
    | isPrefix "image/" (file ^. fileMime) -> do
        let magick = proc "magick" ["identify", "-auto-orient", "-format", "%w %h", file ^. filePath]
        (_, content) <- liftIO $ runStdout magick
        let parser = (,) <$> decimal <*> (char ' ' *> decimal)
        case runParser parser () "" content of
          Right (width, height) ->
            let w = width :: Int
             in let h = height :: Int
                 in writeJsonToFile tgt $ object ["width" .= toJSON w, "height" .= toJSON h]
          Left _ -> pure ()
  _ -> pure ()

actionData :: Action -> ActionData
actionData Miniature = ActionData Picture def Nothing
actionData Size = ActionData Json def Nothing
