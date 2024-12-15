module Korrvigs.Web.Git (getGitR, postGitR) where

import Control.Lens
import Data.Text (Text)
import qualified Korrvigs.Utils.Git.Status as St
import Korrvigs.Web.Backend
import Korrvigs.Web.Login (logWrap)
import Yesod

renderStatus :: St.FileStatus -> Widget
renderStatus (St.StatusChanged change) =
  [whamlet|
    <details>
      <summary>#{path} modified
      File changed!
  |]
  where
    path = change ^. St.changePath
renderStatus (St.StatusMoved move) =
  [whamlet|
    <details>
      <summary>#{source} #{o} #{target}
      Score: #{sc}
  |]
  where
    source = move ^. St.moveSource
    target = move ^. St.moveTarget
    (o, sc) = case move ^. St.moveScore of
      St.CopiedScore score -> ("->>" :: Text, score)
      St.MovedScore score -> ("->", score)
renderStatus (St.StatusUnknown path) =
  [whamlet|
    <details>
      <summary>#{path} created
      New file!
    |]
renderStatus (St.StatusIgnored _) = mempty

getGitR :: Handler Html
getGitR = do
  statuses <- St.gitStatusKorr
  logWrap $
    defaultLayout
      [whamlet|
  <h1>Status
  $forall st <- statuses
    ^{renderStatus st}
  |]

postGitR :: Handler Html
postGitR = pure undefined
