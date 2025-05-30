module Korrvigs.Web.Actions.Share where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Public.Crypto as Public
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Search.Form as Search
import Yesod

shareTarget :: ActionTarget -> Bool
shareTarget (TargetEntry _) = True
shareTarget TargetHome = False
shareTarget (TargetSearch _ _) = True
shareTarget (TargetNoteCollection _ _) = True

shareForm :: AForm Handler ()
shareForm = pure ()

shareTitle :: ActionTarget -> Text
shareTitle = const "Share"

runShare :: () -> ActionTarget -> Handler ActionReaction
runShare () (TargetEntry entry) = do
  public <- Public.signRoute (EntryR $ WId i) []
  publicDl <- Public.signRoute (EntryDownloadR $ WId i) []
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
runShare () (TargetSearch q disp) = do
  let params = Search.getParameters Nothing q disp
  public <- Public.signRoute SearchR params
  render <- getUrlRenderParams
  let html = htmlUrl public params render render
  pure $ def & reactMsg ?~ html
  where
    htmlUrl public params render =
      [hamlet|
        <ul>
          <li>
            <a href=#{render (PublicSearchR public) params}>
              Share this query
      |]
runShare () (TargetNoteCollection note col) = do
  public <- Public.signRoute (NoteColR (WId i) col) []
  render <- getUrlRenderParams
  let html = htmlUrl public render
  pure $ def & reactMsg ?~ html
  where
    i = note ^. noteEntry . name
    htmlUrl public =
      [hamlet|
        <ul>
          <li>
            <a href=@{PublicNoteColR public (WId i) col}>
              Share this collection
      |]
