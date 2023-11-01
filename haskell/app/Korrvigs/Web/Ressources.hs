module Korrvigs.Web.Ressources where

import Korrvigs.Web.Backend
import Yesod

jquery :: Backend a => WidgetFor a ()
jquery =
  addScriptRemoteAttrs
    "https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js"
    [("integrity", "sha384-1H217gwSVyLSIfaLxHbE7dRb3v4mYCKbpQvzx0cegeju1MVsGrX5xXxAvs/HgeFs"), ("crossorigin", "anonymous")]

fuzzy :: Backend a => WidgetFor a ()
fuzzy =
  addScriptRemoteAttrs
    "https://cdn.jsdelivr.net/npm/fuse.js@7.0.0"
    [("integrity", "sha384-PCSoOZTpbkikBEtd/+uV3WNdc676i9KUf01KOA8CnJotvlx8rRrETbDuwdjqTYvt"), ("crossorigin", "anonymous")]
