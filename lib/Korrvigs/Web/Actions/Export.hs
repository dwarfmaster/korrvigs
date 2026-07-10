module Korrvigs.Web.Actions.Export where

import Control.Lens
import Data.Aeson.Decoding
import Data.Aeson.Text (encodeToLazyText)
import Data.Default
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Note
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Search.Form
import Yesod

exportTarget :: ActionTarget -> ActionCond
exportTarget (TargetSearch _ _) = ActCondAlways
exportTarget _ = ActCondNever

importTarget :: ActionTarget -> ActionCond
importTarget (TargetSearch _ _) = ActCondAlways
importTarget _ = ActCondNever

exportForm :: AForm Handler ()
exportForm = pure ()

importForm :: AForm Handler (Collection, Text)
importForm =
  (,)
    <$> areq displayResultsField "display" Nothing
    <*> areq textField "query" Nothing

exportTitle :: ActionTarget -> Text
exportTitle = const "Export"

importTitle :: ActionTarget -> Text
importTitle = const "Import"

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

runImport :: (Collection, Text) -> ActionTarget -> Handler ActionReaction
runImport (col, queryTxt) (TargetSearch _ _) = do
  let mquery = eitherDecode $ LEnc.encodeUtf8 $ LT.fromStrict queryTxt
  case mquery of
    Left err ->
      pure $ def & reactMsg ?~ [shamlet|<p>Failed to decode: #{err}|]
    Right query -> do
      let params = getParameters Nothing query col
      render <- getUrlRenderParams
      pure $ def & reactRedirect ?~ render SearchR params
runImport _ _ = pure def
