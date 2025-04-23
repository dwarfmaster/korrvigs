module Korrvigs.Web.Actions.Share where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Public.Crypto as Public
import Korrvigs.Web.Routes
import Yesod

shareTarget :: ActionTarget -> Bool
shareTarget (TargetEntry _) = True
shareTarget TargetHome = False
shareTarget (TargetCollection _) = True

shareForm :: AForm Handler ()
shareForm = pure ()

shareTitle :: ActionTarget -> Text
shareTitle = const "Share"

runShare :: () -> ActionTarget -> Handler ActionReaction
runShare () (TargetEntry entry) = do
  public <- Public.signRoute $ EntryR $ WId i
  publicDl <- Public.signRoute $ EntryDownloadR $ WId i
  render <- getUrlRenderParams
  let html = htmlUrl public publicDl render
  pure $ def & reactMsg ?~ html
  where
    i = entry ^. name
    htmlUrl public publicDl =
      [hamlet|
      <ul>
        <li>
          <a href=@{PublicEntryR public $ WId i}>Share this entry
        <li>
          <a href=@{PublicEntryDownloadR publicDl $ WId i}>Share the content of this entry
    |]
runShare () TargetHome = pure def
runShare () (TargetCollection col) = do
  public <- Public.signRoute $ ColR col
  render <- getUrlRenderParams
  let html = htmlUrl public render
  pure $ def & reactMsg ?~ html
  where
    htmlUrl public =
      [hamlet|
      <p>
        <a href=@{PublicColR public col}>
          Share
    |]
