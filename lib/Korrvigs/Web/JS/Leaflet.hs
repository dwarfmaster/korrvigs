module Korrvigs.Web.JS.Leaflet
  ( MapItem (..),
    mitGeo,
    mitContent,
    mitVar,
    mitColor,
    MapLayer (..),
    mlayUrl,
    mlayMinZoom,
    mlayMaxZoom,
    mlayBounds,
    leafletWidget,
    jsPoint,
  )
where

import Control.Lens hiding ((.=))
import Data.Foldable
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
    _mitContent :: Maybe Html,
    _mitVar :: Maybe Text,
    _mitColor :: Maybe Text
  }

data MapLayer = MapLayer
  { _mlayUrl :: Text,
    _mlayMinZoom :: Maybe Int,
    _mlayMaxZoom :: Maybe Int,
    _mlayBounds :: Maybe (Point, Point)
  }

makeLenses ''MapItem
makeLenses ''MapLayer

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

defaultCenter :: Point
defaultCenter = V2 2.359775 48.915611

computeCenter :: [Geometry] -> Point
computeCenter [] = defaultCenter
computeCenter geoms = sum pts / V2 npts npts
  where
    pts :: [V2 Double]
    pts = concatMap points geoms
    npts :: Double
    npts = fromIntegral $ length pts

computeBounds :: [Geometry] -> (Point, Point)
computeBounds geoms = case pts of
  [] -> (defaultCenter, defaultCenter)
  (pt : opts) -> foldr insertPoint (pt, pt) opts
  where
    pts :: [V2 Double]
    pts = geoms >>= points
    insertPoint :: Point -> (Point, Point) -> (Point, Point)
    insertPoint pt (mn, mx) =
      ( V2 (min (pt ^. _x) (mn ^. _x)) (min (pt ^. _y) (mn ^. _y)),
        V2 (max (pt ^. _x) (mx ^. _x)) (max (pt ^. _y) (mx ^. _y))
      )

leafletWidget :: Text -> [MapLayer] -> [MapItem] -> Widget
leafletWidget i layers items = do
  let mp = rawJS i
  Rcs.leaflet StaticR
  [whamlet|<div ##{i}>|]
  let geos =
        (view mitGeo <$> items)
          ++ (layers >>= (toList . view mlayBounds) >>= (\bds -> [GeoPoint (fst bds), GeoPoint (snd bds)]))
  let (V2 centerY centerX) = computeCenter geos
  let (V2 boundMinY boundMinX, V2 boundMaxY boundMaxX) = computeBounds geos
  toWidget
    [julius|
    var #{mp} = L.map(#{i}).setView([#{rawJS $ show centerX}, #{rawJS $ show centerY}], 13)
    if(!window.hasOwnProperty("updateFolded")) {
      window.updateFolded = []
    }
    updateFolded.push(function () {
      if(#{mp}.getSize().equals(L.point(0,0))) {
        #{mp}.invalidateSize()
        if(!#{mp}.getSize().equals(L.point(0,0))) {
          #{mp}.fitBounds([[#{rawJS $ show boundMinX}, #{rawJS $ show boundMinY}], [#{rawJS $ show boundMaxX}, #{rawJS $ show boundMaxY}]], { maxZoom: 13 }); 
          return true;
        }
      }
      return false;
    });
    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>',
        referrerPolicy: 'origin'
    }).addTo(#{mp});
    if(!#{mp}.getSize().equals(L.point(0,0))) {
      #{mp}.fitBounds([[#{rawJS $ show boundMinX}, #{rawJS $ show boundMinY}], [#{rawJS $ show boundMaxX}, #{rawJS $ show boundMaxY}]], { maxZoom: 13 }); 
    }
  |]
  forM_ layers $ \layer -> do
    let prepBounds (V2 lat1 lon1, V2 lat2 lon2) = [[lon1, lat1], [lon2, lat2]]
    let opts =
          object $
            catMaybes
              [ ("maxNativeZoom" .=) <$> layer ^. mlayMaxZoom,
                ("minNativeZoom" .=) <$> layer ^. mlayMinZoom,
                ("bounds" .=) . prepBounds <$> layer ^. mlayBounds
              ]
    toWidget [julius|L.tileLayer(#{view mlayUrl layer}, #{opts}).addTo(#{mp});|]
  toWidget
    [cassius|
      ##{i}
        height: 30em
    |]
  forM_ items $ \item -> do
    markerVar <- maybe newIdent pure $ item ^. mitVar
    let color = fromMaybe "#00EE00" $ item ^. mitColor
    case item ^. mitGeo of
      GeoPoint pt ->
        toWidget
          [julius|var #{rawJS markerVar} = L.marker(#{rawJS $ jsPoint pt}).addTo(#{mp});|]
      GeoPath path ->
        toWidget
          [julius|var #{rawJS markerVar} = L.polyline(#{jsPath path}, {color: #{color}}).addTo(#{mp});|]
      GeoPolygon poly ->
        toWidget
          [julius|var #{rawJS markerVar} = L.polygon(#{jsPolygon poly}, {color: #{color}}).addTo(#{mp});|]
    case item ^. mitContent of
      Nothing -> pure ()
      Just content ->
        toWidget
          [julius|
          #{rawJS markerVar}.bindPopup(#{renderHtml content})
        |]
