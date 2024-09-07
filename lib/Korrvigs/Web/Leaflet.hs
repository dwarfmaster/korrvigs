module Korrvigs.Web.Leaflet (MapItem (..), leafletWidget) where

import Control.Lens
import Control.Monad
import Data.Maybe
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
    _mitContent :: Maybe ((Route WebData -> Text) -> Html)
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

leafletWidget :: Maybe Text -> [MapItem] -> Widget
leafletWidget details items = do
  Rcs.leaflet
  detClass <- newIdent
  case details of
    Just title ->
      [whamlet|
      <details .#{detClass}>
        <summary>#{title}
        <div #map>
      |]
    Nothing -> [whamlet|<div #map>|]
  let (V2 centerY centerX) = computeCenter $ (^. mitGeo) <$> items
  toWidget
    [julius|
    var map = L.map('map').setView([#{rawJS $ show centerX}, #{rawJS $ show centerY}], 13)
    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(map);
  |]
  render <- getUrlRender
  forM_ items $ \item -> do
    markerVar <- newIdent
    case item ^. mitGeo of
      GeoPoint pt ->
        toWidget
          [julius|var #{rawJS markerVar} = L.marker(#{rawJS $ jsPoint pt}).addTo(map)|]
      GeoPath path ->
        toWidget
          [julius|var #{rawJS markerVar} = L.polyline(#{jsPath path}).addTo(map)|]
      GeoPolygon poly ->
        toWidget
          [julius|var #{rawJS markerVar} = L.polygon(#{jsPolygon poly}).addTo(map)|]
    case item ^. mitContent of
      Nothing -> pure ()
      Just content ->
        toWidget
          [julius|
          #{rawJS markerVar}.bindPopup(#{renderHtml $ content render})
        |]
  when (isJust details) $
    toWidget
      [julius|
    var details = document.querySelector(#{"." <> detClass})
    details.addEventListener("toggle", function() {
      map.invalidateSize()
    })
  |]
