module Korrvigs.Web.Ressources where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Korrvigs.Utils.Base16
import Korrvigs.Web.Ressources.Helpers
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
entryStyle = toWidget $(cassiusFile $ css "entry.cassius")
