module Korrvigs.File.Mtdt.Pandoc (extract) where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Korrvigs.Utils.Pandoc
import Network.Mime
import Text.Pandoc
import Text.Pandoc.Format

extract :: FilePath -> MimeType -> IO (Map Text Value)
extract path _ = case formatFromFilePaths [path] of
  Nothing -> pure M.empty
  Just format ->
    runIO (readFromFormat path format) >>= \case
      Left _ -> pure M.empty
      Right pd ->
        pure $ pdExtractMtdt pd

readFromFormat :: FilePath -> FlavoredFormat -> PandocIO Pandoc
readFromFormat path format = case lookup name readers of
  Just reader -> do
    exts <- applyExtensionsDiff config format
    let opts = def {readerExtensions = exts}
    case reader of
      TextReader doRead -> liftIO (TIO.readFile path) >>= doRead opts
      ByteStringReader doRead -> liftIO (BS.readFile path) >>= doRead opts
  Nothing -> throwError $ PandocUnknownReaderError name
  where
    config :: ExtensionsConfig
    config = ExtensionsConfig (getDefaultExtensions name) (getAllExtensions name)
    name :: Text
    name = formatName format
