module Korrvigs.File.Mtdt.GPX where

import Control.Lens
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Korrvigs.File.Sync
import Korrvigs.Geometry
import Linear.V2
import Network.Mime
import System.FilePath
import Text.Read (readMaybe)
import qualified Text.XML as XML
import Text.XML.Cursor

extractPt :: XML.Node -> [Point]
extractPt (XML.NodeElement elm) = maybeToList $ do
  let attrs = XML.elementAttributes elm
  lat <- M.lookup "lat" attrs
  latParsed <- readMaybe $ T.unpack lat :: Maybe Double
  lon <- M.lookup "lon" attrs
  lonParsed <- readMaybe $ T.unpack lon :: Maybe Double
  pure $ V2 lonParsed latParsed
extractPt _ = []

extractPoints :: FilePath -> IO [Point]
extractPoints path = do
  xml <- XML.readFile def path
  let cursors = fromDocument xml $// laxElement "trkpt"
  pure $ cursors >>= extractPt . node

extract :: FilePath -> MimeType -> IO (FileMetadata -> FileMetadata)
extract path _
  | takeExtension path == ".gpx" = do
      pts <- extractPoints path
      pure $
        if null pts
          then id
          else exGeo ?~ GeoPath pts
  | otherwise = pure id
