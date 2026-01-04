module Korrvigs.Web.JS.ThreePipe (viewer) where

import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Yesod
import Yesod.Static

viewer :: Route WebData -> Handler Widget
viewer url = pure $ do
  Rcs.threePipeJS StaticR
  toWidget
    [cassius|
    .three-viewer
      width: 100%
      height: 50em
  |]
  [whamlet|
    <canvas .three-viewer three-data=@{url} three-hdr=@{StaticR (StaticRoute ["hdrmap.hdr"] [])}>
  |]
