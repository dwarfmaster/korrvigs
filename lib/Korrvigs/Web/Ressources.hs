module Korrvigs.Web.Ressources where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Korrvigs.Utils.Base16
import Korrvigs.Web.Ressources.Helpers
import Text.Blaze.Html
import Text.Cassius (cassiusFile)
import Yesod
import Yesod.Static

defaultCss :: (Base16Index -> Text) -> WidgetFor site ()
defaultCss base = toWidget $(cassiusFile $ css "default.cassius")

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
  addScript $ mkStatic $ StaticRoute ["js", "leaflet.js"] []
  addStylesheet $ mkStatic $ StaticRoute ["css", "leaflet.css"] []

visNetwork :: (Route Static -> Route site) -> WidgetFor site ()
visNetwork mkStatic =
  addScript $ mkStatic $ StaticRoute ["js", "vis-network.min.js"] []

visTimeline :: (Route Static -> Route site) -> WidgetFor site ()
visTimeline mkStatic =
  addScript $ mkStatic $ StaticRoute ["js", "vis-timeline-graph2d.min.js"] []

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
  addScriptAttrs (mkStatic $ StaticRoute ["js", "mathjax.js"] []) [("id", "Mathjax-Script")]

mtdtCode :: WidgetFor site ()
mtdtCode =
  toWidget $ mkJs $(embedFile $ js "mtdt.js")
