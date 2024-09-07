module Korrvigs.Web.Leaflet (MapItem (..), leafletWidget) where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Text (Text)
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
      GeoPoint (V2 y x) ->
        toWidget
          [julius|
          var #{rawJS markerVar} = L.marker([#{rawJS $ show x}, #{rawJS $ show y}]).addTo(map)
        |]
      GeoPath path -> undefined
      GeoPolygon poly -> undefined
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
