module Korrvigs.Web.Entry.File (content, embed) where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Viking
import Korrvigs.Entry
import Korrvigs.File.Computation (hasModel)
import qualified Korrvigs.File.Mtdt.FIT as FIT
import qualified Korrvigs.File.Mtdt.GPX as GPX
import Korrvigs.Geometry
import Korrvigs.Utils
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.JS.Foliate as Foliate
import Korrvigs.Web.JS.Leaflet
import qualified Korrvigs.Web.JS.ThreePipe as ThreePipe
import Korrvigs.Web.Routes (WebId (WId))
import qualified Korrvigs.Web.Widgets as Widgets
import Linear.V2
import Network.Mime
import Yesod

statusWidget :: FileStatus -> Widget
statusWidget = \case
  FilePlain -> widget "plain"
  FilePresent -> widget "present"
  FileAbsent -> widget "absent"
  where
    widget :: Text -> Widget
    widget t =
      [whamlet|
      <p>
        <em>Status:
        #{t}
    |]

mimeWidget :: MimeType -> Widget
mimeWidget mime =
  [whamlet|
  <p>
    <em>
      Mime:
    #{Enc.decodeUtf8 mime}
|]

sourceFor :: File -> Widget
sourceFor file =
  let i = file ^. fileEntry . entryName
   in let mime = Enc.decodeUtf8 $ file ^. fileMime
       in [whamlet|<source type=#{mime} src=@{EntryDownloadR $ WId i}>|]

audioWidget :: File -> Handler Widget
audioWidget file =
  pure [whamlet|<audio controls>^{sourceFor file}|]

videoWidget :: File -> Handler Widget
videoWidget file =
  pure [whamlet|<video controls width=100%>^{sourceFor file}|]

imgWidget :: File -> Handler Widget
imgWidget file =
  let i = file ^. fileEntry . entryName
   in let url = EntryDownloadR $ WId i
       in pure
            [whamlet|
    <a href=@{url}>
      <img src=@{url} width=100%>
  |]

textWidget :: File -> Handler Widget
textWidget file = do
  txt <- liftIO $ LEnc.decodeUtf8With (\_ -> const $ Just '\xFFFD') <$> LBS.readFile (file ^. filePath)
  pure $ do
    toWidget
      [cassius|
      pre
        padding: 0.5em
        box-sizing: border-box
        width: 100%
        overflow-x: scroll
        background-color: var(--base00)
        color: var(--base07)
      code
        width: 100%
    |]
    [whamlet|
      <pre>
        <code>
          ^{txt}
    |]

pdfWidget :: File -> Handler Widget
pdfWidget file =
  let i = file ^. fileEntry . entryName
   in pure $ Widgets.embedPdf $ EntryDownloadR $ WId i

gpxWidget :: File -> Handler Widget
gpxWidget file = do
  pts <- liftIO $ GPX.extractPoints $ file ^. filePath
  i <- newIdent
  pure $ leafletWidget i [MapItem (GeoPath pts) Nothing Nothing Nothing]

fitWidget :: File -> Handler Widget
fitWidget file = do
  pts <- liftIO $ FIT.extractPoints $ file ^. filePath
  i <- newIdent
  pure $ leafletWidget i [MapItem (GeoPath pts) Nothing Nothing Nothing]

bookWidget :: File -> Handler Widget
bookWidget file =
  Foliate.viewer $ EntryDownloadR $ WId $ file ^. fileEntry . entryName

threeWidget :: Bool -> File -> Handler Widget
threeWidget True file =
  ThreePipe.viewer $ EntryDownloadNamedR (WId $ file ^. fileEntry . entryName) "model.glb"
threeWidget False file =
  ThreePipe.viewer $ EntryComputeNamedR (WId $ file ^. fileEntry . entryName) "model" "model.glb"

vikingWidget :: File -> Handler Widget
vikingWidget file = fromMaybeT mempty $ do
  vik <- hoistEitherLift $ liftIO $ parseVikingFile $ file ^. filePath
  let waypoints = vik ^.. vikTopLayers . each . vikLayers . each . vikLayerWaypoints . each
  let tracks = vik ^.. vikTopLayers . each . vikLayers . each . vikLayerTracks . each
  let mkSegments trk = trackItem (trk ^. vikTrackName) (trk ^. vikTrackColor) <$> (trk ^. vikSegments)
  i <- newIdent
  pure $ leafletWidget i $ (waypointItem <$> waypoints) ++ concatMap mkSegments tracks
  where
    waypointItem :: VikingWayPoint -> MapItem
    waypointItem wp =
      MapItem
        { _mitGeo = GeoPoint $ V2 (wp ^. vikWPLon) (wp ^. vikWPLat),
          _mitContent = Just [shamlet|<p>#{view vikWPName wp}|],
          _mitVar = Nothing,
          _mitColor = Nothing
        }
    trackItem :: Text -> Text -> [VikingTrackPoint] -> MapItem
    trackItem name color segment =
      MapItem
        { _mitGeo = GeoPath $ segment <&> \tp -> V2 (tp ^. vikTPLon) (tp ^. vikTPLat),
          _mitContent = Just [shamlet|<p>#{name}|],
          _mitVar = Nothing,
          _mitColor = Just $ "#" <> color
        }

embed :: Int -> File -> Handler Widget
embed _ file
  | file ^. fileStatus == FileAbsent = pure $ do
      cid <- newIdent
      toWidget
        [cassius|
        ##{cid}
          overflow-x: scroll
      |]
      [whamlet|
        <div ##{cid}>
          <code>#{file ^. filePath}
      |]
embed _ file = do
  let mime = file ^. fileMime
  fromMaybe (pure mempty) $
    lookup
      True
      [ (BS.isPrefixOf "audio/" mime, audioWidget file),
        (BS.isPrefixOf "video/" mime, videoWidget file),
        (BS.isPrefixOf "image/" mime, imgWidget file),
        (BS.isPrefixOf "text/" mime, textWidget file),
        (mime == "application/pdf", pdfWidget file),
        (mime == "application/gpx+xml", gpxWidget file),
        (mime == "application/fits", fitWidget file),
        (BS.isPrefixOf "application/epub" mime, bookWidget file),
        (mime == "application/vnd.comicbook+zip", bookWidget file),
        ("model/gltf-binary" == mime, threeWidget True file),
        (mime == "application/x-viking", vikingWidget file),
        (hasModel mime, threeWidget False file)
      ]

content :: File -> Handler Widget
content file = do
  embeded <- embed 0 file
  pure $ do
    statusWidget $ file ^. fileStatus
    mimeWidget $ file ^. fileMime
    embeded
