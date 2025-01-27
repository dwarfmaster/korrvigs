module Korrvigs.File.Mtdt.PdfToText (extract) where

import Control.Lens
import Control.Monad
import Data.Char
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Korrvigs.File.Sync
import Network.Mime
import System.IO
import System.IO.Temp
import System.Process

extract :: FilePath -> MimeType -> IO (FileMetadata -> FileMetadata)
extract path "application/pdf" =
  withSystemTempFile "korrvigsExtract" $ \tmp handle -> do
    hClose handle
    let pdftotext = proc "pdftotext" [path, tmp]
    devNull <- openFile "/dev/null" WriteMode
    (_, _, _, prc) <- createProcess pdftotext {std_out = UseHandle devNull, std_err = UseHandle devNull}
    void $ waitForProcess prc
    txt <- TLIO.readFile tmp
    let final = TL.filter (\c -> isAlphaNum c || isSpace c) txt
    let str = TL.toStrict final
    pure $ exText %~ Just . maybe str (<> str)
extract _ _ = pure id
