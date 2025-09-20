module Korrvigs.Web.Actions.Export where

import Control.Lens
import Data.Aeson.Text (encodeToLazyText)
import Data.Default
import Data.Text (Text)
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Yesod

exportTarget :: ActionTarget -> ActionCond
exportTarget (TargetSearch _ _) = ActCondAlways
exportTarget _ = ActCondNever

exportForm :: AForm Handler ()
exportForm = pure ()

exportTitle :: ActionTarget -> Text
exportTitle = const "Export"

runExport :: () -> ActionTarget -> Handler ActionReaction
runExport () (TargetSearch query _) = do
  render <- getUrlRenderParams
  let js = encodeToLazyText query
  let html = htmlUrl js render
  pure $ def & reactMsg ?~ html
  where
    htmlUrl js =
      [hamlet|
      <pre style="width:100%;background-color:var(--base00);overflow-y:scroll;padding-bottom:1em">
        <code>
          #{js}
    |]
runExport () _ = pure def
