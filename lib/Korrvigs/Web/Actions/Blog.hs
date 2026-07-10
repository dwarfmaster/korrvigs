module Korrvigs.Web.Actions.Blog where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Data.Time
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Blog
import Korrvigs.Monad.Metadata
import Korrvigs.Query
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Yesod

blogpostQuery :: Query
blogpostQuery =
  def
    & queryMtdt .~ [(mtdtSqlName BlogPost, AnyQuery)]
    & queryKind .~ Just (KindQueryNote NoteQuery)

publishBlogTarget :: ActionTarget -> ActionCond
publishBlogTarget (TargetEntry _) =
  ActCondAnd
    [ ActCondQuery blogpostQuery,
      ActCondQuery (def & queryMtdt .~ [(mtdtSqlName BlogPost, AnyQuery)])
    ]
publishBlogTarget _ = ActCondNever

previewBlogTarget :: ActionTarget -> ActionCond
previewBlogTarget (TargetEntry _) = ActCondQuery blogpostQuery
previewBlogTarget TargetHome = ActCondAlways
previewBlogTarget _ = ActCondNever

publishBlogForm :: AForm Handler ()
publishBlogForm = pure ()

previewBlogForm :: AForm Handler ()
previewBlogForm = pure ()

publishBlogTitle :: ActionTarget -> Text
publishBlogTitle = const "Publish"

previewBlogTitle :: ActionTarget -> Text
previewBlogTitle = const "Preview"

runPublishBlog :: () -> ActionTarget -> Handler ActionReaction
runPublishBlog () (TargetEntry entry) = do
  time <- liftIO getCurrentTime
  tz <- liftIO getCurrentTimeZone
  updateDate entry $ Just $ ZonedTime (utcToLocalTime tz time) tz
  pure $ def & reactMsg ?~ html
  where
    html =
      [shamlet|
      <p>Published blog post !
    |]
runPublishBlog () _ = pure def

runPreviewBlog :: () -> ActionTarget -> Handler ActionReaction
runPreviewBlog () TargetHome = do
  render <- getUrlRenderParams
  pure $ def & reactMsg ?~ htmlUrl render
  where
    htmlUrl =
      [hamlet|
      <ul>
        <li>
          <a href=@{BlogR}>Blog main page
        <li>
          <a href=@{BlogArchiveAllR}>Archive
        <li>
          <a href=@{BlogAtomAllR}>RSS
    |]
runPreviewBlog () (TargetEntry entry) = do
  blogpost <- rSelectTextMtdt BlogPost $ sqlId $ entry ^. entryName
  tags <- rSelectListMtdt BlogTags $ sqlId $ entry ^. entryName
  render <- getUrlRenderParams
  pure $ def & reactMsg ?~ html blogpost tags render
  where
    html blogpost tags =
      [hamlet|
      <ul>
        $maybe b <- blogpost
          <li>
            <a href=@{BlogPostR b}>Post
        $forall tag <- tags
          <li>
            <a href=@{BlogArchiveTagR tag}>
              Archive for #{tag}
            (
            <a href=@{BlogAtomTagR tag}>
              RSS
            )
    |]
runPreviewBlog () _ = pure def
