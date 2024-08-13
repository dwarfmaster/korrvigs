module Korrvigs.File.Mtdt (extractMetadata) where

import Control.Concurrent.Async
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Korrvigs.File.Mtdt.ExifTool as ExifTool
import qualified Korrvigs.File.Mtdt.Pandoc as Pandoc
import qualified Korrvigs.File.Mtdt.PdfToText as PdfToText
import Network.Mime

extractMetadata :: FilePath -> MimeType -> IO (Map Text Value)
extractMetadata path mime = do
  mps <-
    mapConcurrently
      process
      [ ExifTool.extract,
        PdfToText.extract,
        Pandoc.extract
      ]
  pure $ mconcat mps
  where
    process :: (FilePath -> MimeType -> IO (Map Text Value)) -> IO (Map Text Value)
    process ext = ext path mime
