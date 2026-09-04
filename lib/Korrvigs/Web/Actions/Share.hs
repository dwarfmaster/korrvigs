module Korrvigs.Web.Actions.Share where

import Control.Lens
import Control.Monad
import Control.Monad.Extra (whenMaybe)
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Blog
import Korrvigs.Monad
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Public.Crypto as Public
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Search.Form as Search
import Opaleye
import qualified Opaleye as O
import Yesod

shareTarget :: ActionTarget -> ActionCond
shareTarget (TargetEntry _) = ActCondAlways
shareTarget TargetHome = ActCondNever
shareTarget (TargetSearch _ _) = ActCondAlways
shareTarget (TargetNoteCollection _ _) = ActCondAlways
shareTarget (TargetNoteSub _ _) = ActCondAlways
shareTarget (TargetNoteCode _ _) = ActCondAlways

shareForm :: AForm Handler (Maybe Day)
shareForm = aopt dayField "Until" Nothing

shareTitle :: ActionTarget -> Text
shareTitle = const "Share"

runShare :: Maybe Day -> ActionTarget -> Handler ActionReaction
runShare deadline (TargetEntry entry) = do
  public <- Public.mkPublicAlways (EntryR $ WId i) [] deadline
  publicDl <- Public.mkPublicAlways (EntryDownloadR $ WId i) [] deadline
  render <- getUrlRenderParams
  blogpost <- rSelectMtdt BlogPost $ sqlId $ entry ^. entryName
  signedpost <- forM blogpost $ \post -> do
    Public.mkPublicAlways (BlogPostR post) [] deadline
  let html = htmlUrl public publicDl signedpost render
  pure $ def & reactMsg ?~ html
  where
    i = entry ^. entryName
    htmlUrl public publicDl signedpost =
      [hamlet|
      <ul>
        <li>
          <a href=@{public}>Share this entry
        <li>
          <a href=@{publicDl}>Share the content of this entry
        $maybe post <- signedpost
          <li>
            <a href=@{post}>
              Share the blogpost
    |]
runShare _ TargetHome = pure def
runShare deadline (TargetSearch q disp) = do
  let params = Search.getParameters Nothing q disp
  public <- Public.mkPublicAlways SearchR params deadline
  render <- getUrlRenderParams
  let html = htmlUrl public params render render
  pure $ def & reactMsg ?~ html
  where
    htmlUrl public params render =
      [hamlet|
        <ul>
          <li>
            <a href=#{render public params}>
              Share this query
      |]
runShare deadline (TargetNoteCollection note col) = do
  public <- Public.mkPublicAlways (NoteColR (WId i) col) [] deadline
  render <- getUrlRenderParams
  let html = htmlUrl public render
  pure $ def & reactMsg ?~ html
  where
    i = note ^. noteEntry . entryName
    htmlUrl public =
      [hamlet|
        <ul>
          <li>
            <a href=@{public}>
              Share this collection
      |]
runShare deadline (TargetNoteSub note sb) = do
  public <- Public.mkPublicAlways (NoteNamedSubR (WId i) sb) [] deadline
  render <- getUrlRenderParams
  let html = htmlUrl public render
  pure $ def & reactMsg ?~ html
  where
    i = note ^. noteEntry . entryName
    htmlUrl public =
      [hamlet|
        <ul>
          <li>
            <a href=@{public}>
              Share this subtree
      |]
runShare deadline (TargetNoteCode note cd) = do
  public <- Public.mkPublicAlways (NoteNamedCodeR (WId i) cd) [] deadline
  isCached <- fmap isJust $ rSelectOne $ do
    cmp <- selComp (sqlInt4 $ note ^. noteEntry . entryId) cd
    where_ $ O.not $ isNull $ cmp ^. sqlCompLastRun
    pure ()
  publicCache <- whenMaybe isCached $ Public.mkPublicAlways (EntryComputeR (WId i) cd) [] deadline
  render <- getUrlRenderParams
  let html = htmlUrl public publicCache render
  pure $ def & reactMsg ?~ html
  where
    i = note ^. noteEntry . entryName
    htmlUrl public publicCache =
      [hamlet|
        <ul>
          <li>
            <a href=@{public}>
              Share this code block
            $maybe cache <- publicCache
              <li>
                <a href=@{cache}>
                  Share the computation result
      |]
