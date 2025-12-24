module Korrvigs.Web.Entry.File (content, embed) where

import Control.Lens
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.IO as TIO
import Korrvigs.Entry
import qualified Korrvigs.File.Mtdt.GPX as GPX
import Korrvigs.Geometry
import Korrvigs.Web.Backend
import Korrvigs.Web.Leaflet
import Korrvigs.Web.Routes (WebId (WId))
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
  txt <- liftIO $ TIO.readFile $ file ^. filePath
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
   in pure [whamlet|<embed src=@{EntryDownloadR $ WId i} width=100% height=700 type="application/pdf">|]

gpxWidget :: File -> Handler Widget
gpxWidget file = do
  pts <- liftIO $ GPX.extractPoints $ file ^. filePath
  i <- newIdent
  pure $ leafletWidget i [MapItem (GeoPath pts) Nothing Nothing]

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
        (mime == "application/gpx+xml", gpxWidget file)
      ]

content :: File -> Handler Widget
content file = do
  embeded <- embed 0 file
  pure $ do
    statusWidget $ file ^. fileStatus
    embeded
