module Korrvigs.Web.Ressources where

import Korrvigs.Web.Backend
import Text.Cassius (cassiusFile)
import Text.Julius (juliusFile)
import Yesod

fuzzy :: Widget
fuzzy =
  addScriptRemoteAttrs
    "https://cdn.jsdelivr.net/npm/fuse.js@7.0.0/dist/fuse.basic.min.js"
    [("integrity", "sha384-ScL3u6ZEqJiHfmlAb5knv4HAFbMNQRTHmcOJmuGfotCf1v1NtrIQTG9Hd5P843TL"), ("crossorigin", "anonymous")]

entrySelect :: Widget
entrySelect = do
  toWidget $(juliusFile "app/Korrvigs/Web/Ressources/js/entries.julius")
  toWidget $(cassiusFile "app/Korrvigs/Web/Ressources/css/entries.cassius")
