module Korrvigs.Web.Utils (htmlKind) where

import Data.Text (Text)
import Korrvigs.Kind
import Korrvigs.Utils.Base16
import Korrvigs.Web.Backend
import Yesod

mkHtmlKind :: Text -> Base16Index -> Widget
mkHtmlKind nm col = do
  theme <- handlerToWidget getBase
  [whamlet|
    <span .entry-kind style="background-color:#{theme col}">
      #{nm}
  |]

htmlKind :: Kind -> Widget
htmlKind Note = mkHtmlKind "Note" Base08
htmlKind File = mkHtmlKind "File" Base09
htmlKind Link = mkHtmlKind "Link" Base0A
