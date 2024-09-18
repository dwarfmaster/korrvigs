module Korrvigs.Web.Leaflet (MapItem (..), mitGeo, mitContent, mitVar, leafletWidget, jsPoint) where

import Control.Lens
import Control.Monad
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Bld
import Korrvigs.Geometry
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Linear.V2
import Text.Blaze.Html.Renderer.Text
import Text.Julius
import Yesod

data MapItem = MapItem
  { _mitGeo :: Geometry,
    _mitContent :: Maybe Html,
    _mitVar :: Maybe Text
  }

makeLenses ''MapItem

points :: Geometry -> [V2 Double]
points (GeoPoint pt) = [pt]
points (GeoPath path) = path
points (GeoPolygon (Polygon path _)) = path

jsList' :: (a -> Bld.Builder) -> [a] -> Bld.Builder
jsList' _ [] = "]"
jsList' rdr [x] = rdr x <> "]"
jsList' rdr (x : xs) = rdr x <> "," <> jsList' rdr xs

jsList :: (a -> Bld.Builder) -> [a] -> Bld.Builder
jsList rdr xs = "[" <> jsList' rdr xs

jsPoint :: Point -> Bld.Builder
jsPoint (V2 y x) =
  "[" <> Bld.fromString (show x) <> "," <> Bld.fromString (show y) <> "]"

jsPath :: Path -> RawJavascript
jsPath = rawJS . jsList jsPoint

jsPolygon :: Polygon -> RawJavascript
jsPolygon (Polygon poly hls) = rawJS $ jsList (jsList jsPoint) $ poly : hls

computeCenter :: [Geometry] -> Point
computeCenter geoms = sum pts / V2 npts npts
  where
    pts :: [V2 Double]
    pts = concatMap points geoms
    npts :: Double
    npts = fromIntegral $ length pts

leafletWidget :: Text -> [MapItem] -> Widget
leafletWidget i items = do
  let mp = rawJS i
  Rcs.leaflet
  [whamlet|<div ##{i}>|]
  let (V2 centerY centerX) = computeCenter $ (^. mitGeo) <$> items
  toWidget
    [julius|
    var #{mp} = L.map(#{i}).setView([#{rawJS $ show centerX}, #{rawJS $ show centerY}], 13)
    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(#{mp});
  |]
  toWidget
    [cassius|
      ##{i}
        height: 30em
    |]
  forM_ items $ \item -> do
    markerVar <- maybe newIdent pure $ item ^. mitVar
    case item ^. mitGeo of
      GeoPoint pt ->
        toWidget
          [julius|var #{rawJS markerVar} = L.marker(#{rawJS $ jsPoint pt}).addTo(#{mp})|]
      GeoPath path ->
        toWidget
          [julius|var #{rawJS markerVar} = L.polyline(#{jsPath path}).addTo(#{mp})|]
      GeoPolygon poly ->
        toWidget
          [julius|var #{rawJS markerVar} = L.polygon(#{jsPolygon poly}).addTo(#{mp})|]
    case item ^. mitContent of
      Nothing -> pure ()
      Just content ->
        toWidget
          [julius|
          #{rawJS markerVar}.bindPopup(#{renderHtml content})
        |]
