module Korrvigs.File.Mtdt.Pandoc (extract) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Korrvigs.Entry.JSON as Gen
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
        let applyGeom = maybe id (genData . Gen.ejsGeo ?~) (fromJSON' =<< mtdt ^. at "geometry")
        let applyDate = maybe id (genData . Gen.ejsDate ?~) (fromJSON' =<< mtdt ^. at "date")
        let applyDuration = maybe id (genData . Gen.ejsDuration ?~) (fromJSON' =<< mtdt ^. at "duration")
        let applyTxt = if T.null txt then id else genData . Gen.ejsText ?~ txt
        let applyParents = maybe id ((genData . Gen.ejsParents .~)) (fromJSON' =<< mtdt ^. at "parents")
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
