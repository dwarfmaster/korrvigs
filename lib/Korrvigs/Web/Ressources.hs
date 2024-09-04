module Korrvigs.Web.Ressources where

import Data.Text (Text)
import Korrvigs.Web.Ressources.Helpers
import Text.Cassius (cassiusFile)
import Yesod

defaultCss :: (Int -> Text) -> WidgetFor site ()
defaultCss base = toWidget $(cassiusFile $ css "default.cassius")
