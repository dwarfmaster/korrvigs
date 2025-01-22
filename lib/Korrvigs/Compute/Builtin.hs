module Korrvigs.Compute.Builtin where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils.Process
import Network.Mime (MimeType)
import System.Process

data Action = Miniature
  deriving (Eq, Show)

makePrisms ''Action

instance ToJSON Action where
  toJSON Miniature = String "miniature"

instance FromJSON Action where
  parseJSON (String "miniature") = pure Miniature
  parseJSON v = unexpected v

isPrefix :: Text -> MimeType -> Bool
isPrefix prefix mime = T.isPrefixOf prefix $ Enc.decodeASCII mime

run :: (MonadKorrvigs m) => Action -> Entry -> FilePath -> m ()
run Miniature entry tgt = case entry ^. kindData of
  FileD file | file ^. fileStatus /= FilePresent -> pure ()
  FileD file
    | isPrefix "image/" (file ^. fileMime) ->
        let magick = proc "magick" [file ^. filePath, "-resize", "200x200", tgt]
         in liftIO $ void $ runSilent magick
  FileD file
    | isPrefix "video/" (file ^. fileMime) ->
        let ffmpeg = proc "ffmpeg" ["-i", file ^. filePath, "-vframes", "1", "-f", "image2", "-vf", "scale=200:-2", tgt]
         in liftIO $ void $ runSilent ffmpeg
  _ -> pure ()
