module Korrvigs.Web.Utils (colorKind, htmlKind, htmlKind') where

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

colorKind :: Kind -> Base16Index
colorKind Note = Base08
colorKind File = Base09
colorKind Link = Base0A

htmlKind' :: Kind -> Handler Html
htmlKind' Note = mkHtmlKind "Note" $ colorKind Note
htmlKind' File = mkHtmlKind "File" $ colorKind File
htmlKind' Link = mkHtmlKind "Link" $ colorKind Link

htmlKind :: Kind -> Widget
htmlKind kd = do
  html <- handlerToWidget $ htmlKind' kd
  toWidget html
