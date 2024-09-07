module Korrvigs.Web.Leaflet where

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Korrvigs.Geometry
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Yesod

data MapItem = MapItem
  { _mitGeo :: Geometry,
    _mitContent :: Maybe Html
  }

makeLenses ''MapItem

leafletWidget :: Maybe Text -> [MapItem] -> Widget
leafletWidget details _ = do
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
  toWidget
    [julius|
    var map = L.map('map').setView([51.505, -0.09], 13)
    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(map);
  |]
  when (isJust details) $
    toWidget
      [julius|
    var details = document.querySelector(#{"." <> detClass})
    details.addEventListener("toggle", function() {
      map.invalidateSize()
    })
  |]
