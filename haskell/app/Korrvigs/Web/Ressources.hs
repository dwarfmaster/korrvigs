module Korrvigs.Web.Ressources where

import Data.Text (Text)
import Data.UUID (UUID)
import Korrvigs.Classes
import Korrvigs.Classes.Colors
import Korrvigs.Definition
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.UUID as U
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

entryView :: Text -> Maybe Text -> Maybe Class -> [(String, Widget)] -> Widget
entryView title err root fragments = do
  toWidget $(cassiusFile "app/Korrvigs/Web/Ressources/css/entry.cassius")
  toWidget $(whamletFile "app/Korrvigs/Web/Ressources/html/entry.hamlet")
  toWidget $(juliusFile "app/Korrvigs/Web/Ressources/js/entry.julius")
  where
    mkBase :: Class -> String
    mkBase = ("--base" ++) . classBase

classTree :: Widget
classTree = do
  toWidget $(cassiusFile "app/Korrvigs/Web/Ressources/css/classTree.cassius")
  toWidget $(juliusFile "app/Korrvigs/Web/Ressources/js/classTree.julius")

classTreeSub :: Text -> UUID -> Bool -> [Widget] -> Widget
classTreeSub className classUuid folded children = do
  toWidget $(whamletFile "app/Korrvigs/Web/Ressources/html/classTree.hamlet")
  where
    classEntry = U.UUID classUuid

classInstances :: [Entry] -> Widget
classInstances instances = do
  toWidget $(whamletFile "app/Korrvigs/Web/Ressources/html/classInstances.hamlet")

formStyle :: Widget
formStyle = toWidget $(cassiusFile "app/Korrvigs/Web/Ressources/css/forms.cassius")