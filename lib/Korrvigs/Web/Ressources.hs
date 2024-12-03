module Korrvigs.Web.Ressources where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Korrvigs.Utils.Base16
import Korrvigs.Web.Ressources.Helpers
import Text.Blaze.Html
import Text.Cassius (cassiusFile)
import Yesod

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

leaflet :: WidgetFor site ()
leaflet = do
  addScriptRemoteAttrs
    "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
    [("integrity", "sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo="), ("crossorigin", "")]
  addStylesheetRemoteAttrs
    "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
    [("integrity", "sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY="), ("crossorigin", "")]

visNetwork :: WidgetFor site ()
visNetwork = do
  addScriptRemoteAttrs
    "https://unpkg.com/vis-network/standalone/umd/vis-network.min.js"
    []

mathjax :: WidgetFor site ()
mathjax = do
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
  addScriptRemoteAttrs
    "https://cdn.jsdelivr.net/npm/mathjax@3.0.1/es5/tex-mml-chtml.js"
    [("id", "MathJax-Script"), ("integrity", "sha384-/1zmJ1mBdfKIOnwPxpdG6yaRrxP6qu3eVYm0cz2nOx+AcL4d3AqEFrwcqGZVVroG"), ("crossorigin", "anonymous")]
