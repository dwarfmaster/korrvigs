module Korrvigs.File.Mtdt.PdfToText (extract) where

import Control.Monad
import Data.Aeson
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Network.Mime
import System.IO
import System.IO.Temp
import System.Process

extract :: FilePath -> MimeType -> IO (Map Text Value)
extract path "application/pdf" =
  withSystemTempFile "korrvigsExtract" $ \tmp handle -> do
    hClose handle
    let pdftotext = proc "pdftotext" [path, tmp]
    devNull <- openFile "/dev/null" WriteMode
    (_, _, _, prc) <- createProcess pdftotext {std_out = UseHandle devNull, std_err = UseHandle devNull}
    void $ waitForProcess prc
    txt <- TLIO.readFile tmp
    let final = TL.filter (\c -> isAlphaNum c || isSpace c) txt
    pure $ M.singleton "textContent" $ toJSON final
extract _ _ = pure M.empty
