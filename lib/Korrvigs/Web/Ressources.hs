module Korrvigs.Web.Ressources where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Korrvigs.Utils.Base16
import Korrvigs.Web.Ressources.Helpers
import Text.Blaze.Html
import Text.Cassius (cassiusFile)
import Yesod
import Yesod.Static

charisFontFamily :: (Route Static -> Route site) -> WidgetFor site ()
charisFontFamily mkStatic =
  toWidget
    [cassius|
    @font-face
      font-family: CharisSILW
      src: url(@{fontRoute "CharisSIL-Regular.woff2"})
    @font-face
      font-family: CharisSILW
      font-style: italic
      src: url(@{fontRoute "CharisSIL-Italic.woff2"})
    @font-face
      font-family: CharisSILW
      font-weight: bold
      src: url(@{fontRoute "CharisSIL-Bold.woff2"})
    @font-face
      font-family: CharisSILW
      font-weight: bold
      font-style: italic
      src: url(@{fontRoute "CharisSIL-BoldItalic.woff2"})
  |]
  where
    fontRoute name = mkStatic $ StaticRoute ["font", name] []

defaultCss :: (Base16Index -> Text) -> (Route Static -> Route site) -> WidgetFor site ()
defaultCss base mkStatic = do
  charisFontFamily mkStatic
  toWidget $(cassiusFile $ css "default.cassius")

header :: [(Bool, Text, Route site)] -> WidgetFor site ()
header pages = do
  toWidget $(whamletFile $ html "header.hamlet")
  toWidget $(cassiusFile $ css "header.cassius")
  toWidget $ mkJs $(embedFile $ js "header.js")

formsStyle :: WidgetFor site ()
formsStyle = toWidget $(cassiusFile $ css "forms.cassius")

entryStyle :: WidgetFor site ()
entryStyle = do
  toWidget $(cassiusFile $ css "entry.cassius")
  toWidget $(cassiusFile $ css "sidenote.cassius")

leaflet :: (Route Static -> Route site) -> WidgetFor site ()
leaflet mkStatic = do
  addScript $ mkStatic $ StaticRoute ["leaflet", "leaflet.js"] []
  addStylesheet $ mkStatic $ StaticRoute ["leaflet", "leaflet.css"] []

visNetwork :: (Route Static -> Route site) -> WidgetFor site ()
visNetwork mkStatic =
  addScript $ mkStatic $ StaticRoute ["vis", "vis-network.min.js"] []

visTimeline :: (Route Static -> Route site) -> WidgetFor site ()
visTimeline mkStatic =
  addScript $ mkStatic $ StaticRoute ["vis", "vis-timeline-graph2d.min.js"] []

mathjax :: (Route Static -> Route site) -> WidgetFor site ()
mathjax mkStatic = do
  toWidgetHead $
    preEscapedToHtml
      ( "\
        \<script type=\"text/x-mathjax-config\">\
        \  MathJax: {\
        \    tex: {\
        \      inlineMath: [['\\\\(', '\\\\)']],\
        \      processEscapes: true\
        \    }\
        \  }\
        \</script>" ::
          Text
      )
  addScriptAttrs (mkStatic $ StaticRoute ["mathjax", "es5", "tex-mml-chtml.js"] []) [("id", "Mathjax-Script"), ("async", "")]

photoswipe :: (Route Static -> Route site) -> WidgetFor site ()
photoswipe mkStatic =
  addStylesheet $ mkStatic $ StaticRoute ["photoswipe", "photoswipe.css"] []

mtdtCode :: WidgetFor site ()
mtdtCode =
  toWidget $ mkJs $(embedFile $ js "mtdt.js")

actionsCode :: WidgetFor site ()
actionsCode =
  toWidget $ mkJs $(embedFile $ js "actions.js")

checkboxCode :: (Route Static -> Route site) -> WidgetFor site ()
checkboxCode mkStatic = do
  popperJS mkStatic
  toWidget $ mkJs $(embedFile $ js "checkbox.js")

codeMenuCode :: WidgetFor site ()
codeMenuCode = toWidget $ mkJs $(embedFile $ js "code-menu.js")

headerMenuCode :: WidgetFor site ()
headerMenuCode = toWidget $ mkJs $(embedFile $ js "header-menu.js")

ace :: (Route Static -> Route site) -> WidgetFor site ()
ace mkStatic =
  addScript $ mkStatic $ StaticRoute ["ace", "ace.js"] []

fuse :: (Route Static -> Route site) -> WidgetFor site ()
fuse mkStatic =
  addScript $ mkStatic $ StaticRoute ["fuse", "fuse.js"] []

fullcalendar :: (Route Static -> Route site) -> WidgetFor site ()
fullcalendar mkStatic =
  addScript $ mkStatic $ StaticRoute ["fullcalendar", "index.global.min.js"] []

epubJs :: (Route Static -> Route site) -> WidgetFor site ()
epubJs mkStatic = do
  addScript $ mkStatic $ StaticRoute ["jszip", "jszip.min.js"] []
  addScript $ mkStatic $ StaticRoute ["epub", "epub.min.js"] []

popperJS :: (Route Static -> Route site) -> WidgetFor site ()
popperJS mkStatic =
  addScript $ mkStatic $ StaticRoute ["popperjs", "popperjs.min.js"] []

itemCode :: WidgetFor site ()
itemCode =
  toWidget $ mkJs $(embedFile $ js "item.js")

andypfJsonViewer :: (Route Static -> Route site) -> WidgetFor site ()
andypfJsonViewer mkStatic =
  addScript $ mkStatic $ StaticRoute ["andypf", "json-viewer.js"] []

foliateJS :: (Route Static -> Route site) -> WidgetFor site ()
foliateJS mkStatic =
  addScriptAttrs (mkStatic $ StaticRoute ["foliate", "main.js"] []) [("type", "module")]
