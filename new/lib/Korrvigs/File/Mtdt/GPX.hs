module Korrvigs.File.Mtdt.GPX where

import Data.Aeson
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as Enc
import Korrvigs.Geometry
import Korrvigs.Geometry.WKB (writeGeometry)
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

extract :: FilePath -> MimeType -> IO (Map Text Value)
extract path _
  | takeExtension path == ".gpx" = do
      xml <- XML.readFile def path
      let cursors = fromDocument xml $// laxElement "trkpt"
      let pts = cursors >>= extractPt . node
      pure $
        if null pts
          then M.empty
          else M.singleton "geometry" $ toJSON $ Enc.decodeUtf8 $ writeGeometry $ GeoPath pts
  | otherwise = pure M.empty
