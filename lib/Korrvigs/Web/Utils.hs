module Korrvigs.Web.Utils (htmlKind, htmlKind') where

import Data.Text (Text)
import Korrvigs.Kind
import Korrvigs.Utils.Base16
import Korrvigs.Web.Backend
import Yesod

mkHtmlKind :: Text -> Base16Index -> Handler Html
mkHtmlKind nm col = do
  theme <- getBase
  render <- getUrlRender
  let html =
        [hamlet|
    <span .entry-kind style="background-color:#{theme col}">
      #{nm}
  |]
  pure $ html render

htmlKind' :: Kind -> Handler Html
htmlKind' Note = mkHtmlKind "Note" Base08
htmlKind' File = mkHtmlKind "File" Base09
htmlKind' Link = mkHtmlKind "Link" Base0A

htmlKind :: Kind -> Widget
htmlKind kd = do
  html <- handlerToWidget $ htmlKind' kd
  toWidget html
