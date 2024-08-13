module Korrvigs.File.Mtdt (extractMetadata) where

import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Korrvigs.File.Mtdt.ExifTool as ExifTool
import qualified Korrvigs.File.Mtdt.PdfToText as PdfToText
import Network.Mime

extractMetadata :: FilePath -> MimeType -> IO (Map Text Value)
extractMetadata path mime =
  mconcat $
    process
      <$> [ ExifTool.extract,
            PdfToText.extract
          ]
  where
    process :: (FilePath -> MimeType -> IO (Map Text Value)) -> IO (Map Text Value)
    process ext = ext path mime
