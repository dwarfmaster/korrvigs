module Korrvigs.File.Mtdt (extractMetadata, reextractMetadata) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad.IO.Class
import Korrvigs.Entry
import qualified Korrvigs.File.Mtdt.ExifTool as ExifTool
import qualified Korrvigs.File.Mtdt.FIT as FIT
import qualified Korrvigs.File.Mtdt.GPX as GPX
import qualified Korrvigs.File.Mtdt.MBTiles as MBTiles
import qualified Korrvigs.File.Mtdt.Pandoc as Pandoc
import qualified Korrvigs.File.Mtdt.PdfToText as PdfToText
import qualified Korrvigs.File.Mtdt.Viking as Viking
import Korrvigs.File.Sync
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Monad.Sync (syncFileOfKind)
import Network.Mime

type Extractor = FileMetadata -> FileMetadata

extractMetadata :: FilePath -> MimeType -> IO Extractor
extractMetadata path mime = do
  mps <-
    mapConcurrently
      process
      [ FIT.extract,
        PdfToText.extract,
        Pandoc.extract,
        GPX.extract,
        Viking.extract,
        MBTiles.extract,
        ExifTool.extract
      ]
  pure $ foldr (.) id mps
  where
    process :: (FilePath -> MimeType -> IO Extractor) -> IO Extractor
    process ext = ext path mime

reextractMetadata :: (MonadKorrvigs m) => File -> m ()
reextractMetadata file = do
  extractor <- liftIO $ extractMetadata (file ^. filePath) (file ^. fileMime)
  updateImpl file $ pure . extractor
  syncFileOfKind (file ^. fileEntry . entryName) (file ^. filePath) (file ^. fileEntry . entryId) File
