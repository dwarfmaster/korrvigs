module Korrvigs.Web.Entry.File (content, embed) where

import Control.Lens
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import Korrvigs.Entry
import Korrvigs.Web.Backend
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
  let i = file ^. fileEntry . name
   in let mime = Enc.decodeUtf8 $ file ^. fileMime
       in [whamlet|<source type=#{mime} src=@{EntryDownloadR $ WId i}>|]

audioWidget :: File -> Widget
audioWidget file =
  [whamlet|<audio controls>^{sourceFor file}|]

videoWidget :: File -> Widget
videoWidget file =
  [whamlet|<video controls width=100%>^{sourceFor file}|]

imgWidget :: File -> Widget
imgWidget file =
  let i = file ^. fileEntry . name
   in let url = EntryDownloadR $ WId i
       in [whamlet|
    <a href=@{url}>
      <img src=@{url} width=100%>
  |]

textWidget :: File -> Widget
textWidget file =
  let i = file ^. fileEntry . name
   in let mime = Enc.decodeUtf8 $ file ^. fileMime
       in [whamlet|
    <code>
      <object data=@{EntryDownloadR $ WId i} type=#{mime}>
  |]

pdfWidget :: File -> Widget
pdfWidget file =
  let i = file ^. fileEntry . name
   in [whamlet|<embed src=@{EntryDownloadR $ WId i} width=100% height=700 type="application/pdf">|]

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
embed _ file = pure $ do
  let mime = file ^. fileMime
  fromMaybe mempty $
    lookup
      True
      [ (BS.isPrefixOf "audio/" mime, audioWidget file),
        (BS.isPrefixOf "video/" mime, videoWidget file),
        (BS.isPrefixOf "image/" mime, imgWidget file),
        (BS.isPrefixOf "text/" mime, textWidget file),
        (mime == "application/pdf", pdfWidget file)
      ]

content :: File -> Handler Widget
content file = do
  embeded <- embed 0 file
  pure $ do
    statusWidget $ file ^. fileStatus
    embeded
