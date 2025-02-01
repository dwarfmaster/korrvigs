module Korrvigs.File.Mtdt.Pandoc (extract) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Korrvigs.Entry
import Korrvigs.File.Sync
import Korrvigs.Utils.Pandoc
import Network.Mime
import Text.Pandoc
import Text.Pandoc.Format

extract :: FilePath -> MimeType -> IO (FileMetadata -> FileMetadata)
extract path _ = case formatFromFilePaths [path] of
  Nothing -> pure id
  Just format ->
    runIO (readFromFormat path format) >>= \case
      Left _ -> pure id
      Right pd -> do
        let (txt, mtdt) = pdExtractMtdt pd
        let applyGeom = maybe id (exGeo ?~) (fromJSON' =<< mtdt ^. at "geometry")
        let applyDate = maybe id (exDate ?~) (fromJSON' =<< mtdt ^. at "date")
        let applyDuration = maybe id (exDuration ?~) (fromJSON' =<< mtdt ^. at "duration")
        let applyTxt = if T.null txt then id else exText ?~ txt
        let applyParents = maybe id ((exParents .~) . fmap MkId) (fromJSON' =<< mtdt ^. at "parents")
        pure $ applyGeom . applyDate . applyDuration . applyTxt . applyParents

fromJSON' :: (FromJSON a) => Value -> Maybe a
fromJSON' v = case fromJSON v of
  Success x -> Just x
  Error _ -> Nothing

readFromFormat :: FilePath -> FlavoredFormat -> PandocIO Pandoc
readFromFormat path format = case lookup nm readers of
  Just reader -> do
    exts <- applyExtensionsDiff config format
    let opts = def {readerExtensions = exts}
    case reader of
      TextReader doRead -> liftIO (TIO.readFile path) >>= doRead opts
      ByteStringReader doRead -> liftIO (BS.readFile path) >>= doRead opts
  Nothing -> throwError $ PandocUnknownReaderError nm
  where
    config :: ExtensionsConfig
    config = ExtensionsConfig (getDefaultExtensions nm) (getAllExtensions nm)
    nm :: Text
    nm = formatName format
