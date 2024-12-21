module Korrvigs.Web.Git (getGitR, postGitR) where

import Control.Lens
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text.Encoding.Base64
import qualified Korrvigs.Utils.Git.Status as St
import Korrvigs.Web.Backend
import Korrvigs.Web.Login (logWrap)
import System.FilePath
import Text.Cassius
import Yesod hiding (joinPath)

data GitStatus
  = GitNewFile
  | GitDeletedFile
  | GitChangedFile

data FileTree
  = FileTreeDir (Map FilePath FileTree)
  | FileTreeFile FilePath GitStatus

emptyFileTree :: FileTree
emptyFileTree = FileTreeDir M.empty

singletonFileTree :: FilePath -> GitStatus -> [FilePath] -> FileTree
singletonFileTree path st [] = FileTreeFile path st
singletonFileTree path st (part : prts) = FileTreeDir $ M.singleton part $ singletonFileTree path st prts

insertIntoFileTree' :: FilePath -> GitStatus -> [FilePath] -> FileTree -> FileTree
insertIntoFileTree' _ _ _ (FileTreeFile _ _) = error "Invalid insertion into filetree : inserting structure onto already existing file"
insertIntoFileTree' _ _ [] _ = error "Invalid insertion into filetree : inserting file onto already existing structure"
insertIntoFileTree' path st (part : prts) (FileTreeDir subs) = case M.lookup part subs of
  Nothing -> FileTreeDir $ M.insert part (singletonFileTree path st prts) subs
  Just ft -> FileTreeDir $ M.insert part (insertIntoFileTree' path st prts ft) subs

insertIntoFileTree :: FilePath -> GitStatus -> FileTree -> FileTree
insertIntoFileTree path st = insertIntoFileTree' path st comps
  where
    comps :: [FilePath]
    comps = splitDirectories path

extractGitStatus :: St.FileStatus -> [(FilePath, GitStatus)]
extractGitStatus (St.StatusChanged change) = [(change ^. St.changePath, nstatus)]
  where
    nstatus = case (change ^. St.changeIndex, change ^. St.changeTree) of
      (St.Deleted, St.Added) -> GitChangedFile
      (St.Added, _) -> GitNewFile
      (_, St.Added) -> GitNewFile
      (St.Deleted, _) -> GitDeletedFile
      (_, St.Deleted) -> GitDeletedFile
      _ -> GitChangedFile
extractGitStatus (St.StatusMoved move) =
  [ (move ^. St.moveSource, GitDeletedFile),
    (move ^. St.moveTarget, GitNewFile)
  ]
extractGitStatus (St.StatusUnknown path) = [(path, GitNewFile)]
extractGitStatus (St.StatusIgnored _) = []

simplifyFileTree' :: FilePath -> FileTree -> (FilePath, FileTree)
simplifyFileTree' path ft@(FileTreeFile _ _) = (path, ft)
simplifyFileTree' path (FileTreeDir subs) = case M.toList subs of
  [(sub, subv)] -> let (nsub, nft) = simplifyFileTree' sub subv in (joinPath [path, nsub], nft)
  lsubs -> (path, FileTreeDir $ M.fromList $ uncurry simplifyFileTree' <$> lsubs)

simplifyFileTree :: FileTree -> FileTree
simplifyFileTree = snd . simplifyFileTree' ""

gitFileTree :: [St.FileStatus] -> FileTree
gitFileTree statuses = simplifyFileTree $ foldr (uncurry insertIntoFileTree) emptyFileTree $ extractGitStatus =<< statuses

renderSubTree :: FilePath -> FileTree -> Widget
renderSubTree sub ft@(FileTreeDir _) =
  [whamlet|
    <details open>
      <summary>
        <tt>#{sub}
      ^{renderFileTree False ft}
  |]
renderSubTree sub (FileTreeFile _ status) =
  [whamlet|
    <tt *{sym}>#{sub}
  |]
  where
    sym :: [(Text, Text)]
    sym = case status of
      GitNewFile -> [("style", "color:var(--base0B);")]
      GitDeletedFile -> [("style", "color:var(--base08);")]
      GitChangedFile -> []

fileTreeCss :: routes -> Css
fileTreeCss =
  [cassius|
  .gitTree
    --radius: 8px
    --spacing: 1.5rem
    li
      display: block
      position: relative
      padding-left: calc(2 * var(--spacing) - var(--radius) - 2px)
    ul
      padding-left: 0
      margin-left: calc(var(--radius) - var(--spacing))
      li
        border-left: 2px solid var(--base00)
      li:last-child
        border-left: 2px solid var(--base01)
      li::before
        content: ''
        display: block
        position: absolute
        top: calc(var(--spacing) / -2)
        left: -2px
        width: calc(var(--spacing) + 2px)
        height: calc(var(--spacing) + 1px)
        border: solid var(--base00)
        border-width: 0 0 2px 2px
    summary
      display: block
      cursor: pointer
    summary::marker
      display: none
    summary:focus
      outline: none
    summary:focus-visible
      outline: 1px dotted var(--base00)
    li::after
      content: ''
      display: block
      position: absolute
      top: calc(var(--spacing) / 2 - var(--radius))
      left: calc(var(--spacing) - var(--radius) - 1px)
      width: calc(2 * var(--radius))
      height: calc(2 * var(--radius))
      border-radius: 50%
      background: var(--base00)
    summary::before
      content: ''
      display: block
      position: absolute
      top: calc(var(--spacing) / 2 - var(--radius))
      left: calc(var(--spacing) - var(--radius) - 1px)
      width: calc(2 * var(--radius))
      height: calc(2 * var(--radius))
      border-radius: 50%
      background: var(--base00)
    summary::before
      z-index: 1
      background: #696 url('data:image/svg+xml;base64,#{encodeBase64 svg}') 0 0
    details[open] > summary::before
      background-position: calc(-2 * var(--radius)) 0
|]
  where
    svg :: Text
    svg =
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
      \<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"32\" height=\"16\">\
      \  <g fill=\"#fff\" transform=\"scale(0.8)\">\
      \    <path d=\"m5 9h4v-4h2v4h4v2h-4v4h-2v-4h-4z\"/>\
      \    <path d=\"m25 9h10v2h-10z\"/>\
      \  </g>\
      \</svg>"

renderFileTree :: Bool -> FileTree -> Widget
renderFileTree _ (FileTreeFile _ _) = mempty
renderFileTree isTopLevel (FileTreeDir subs) = do
  when isTopLevel $ toWidget fileTreeCss
  [whamlet|
    <ul *{attrs}>
      $forall (path,sub) <- M.toList subs
        <li>
          ^{renderSubTree path sub}
  |]
  where
    attrs :: [(Text, Text)]
    attrs = [("class", "gitTree") | isTopLevel]

getGitR :: Handler Html
getGitR = do
  statuses <- St.gitStatusKorr
  let ft = gitFileTree statuses
  logWrap $
    defaultLayout
      [whamlet|
  <h1>Status
  ^{renderFileTree True ft}
  |]

postGitR :: Handler Html
postGitR = pure undefined
