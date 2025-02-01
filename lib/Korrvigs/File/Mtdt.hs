module Korrvigs.File.Mtdt (extractMetadata) where

import Control.Concurrent.Async
import qualified Korrvigs.File.Mtdt.ExifTool as ExifTool
import qualified Korrvigs.File.Mtdt.GPX as GPX
import qualified Korrvigs.File.Mtdt.Pandoc as Pandoc
import qualified Korrvigs.File.Mtdt.PdfToText as PdfToText
import Korrvigs.File.Sync
import Network.Mime

type Extractor = FileMetadata -> FileMetadata

extractMetadata :: FilePath -> MimeType -> IO Extractor
extractMetadata path mime = do
  mps <-
    mapConcurrently
      process
      [ ExifTool.extract,
        PdfToText.extract,
        Pandoc.extract,
        GPX.extract
      ]
  pure $ foldr (.) id mps
  where
    process :: (FilePath -> MimeType -> IO Extractor) -> IO Extractor
    process ext = ext path mime
