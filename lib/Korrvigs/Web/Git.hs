module Korrvigs.Web.Git (getGitR, postGitR) where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Base64
import Korrvigs.Utils.Base16
import qualified Korrvigs.Utils.Git.Commit as Ci
import qualified Korrvigs.Utils.Git.Status as St
import Korrvigs.Web.Backend
import Korrvigs.Web.Login (logWrap)
import qualified Korrvigs.Web.Ressources as Rcs
import System.FilePath
import Text.Cassius
import Text.Julius
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
simplifyFileTree ft =
  let (path, nft) = simplifyFileTree' "" ft
   in if path == ""
        then nft
        else FileTreeDir $ M.singleton path nft

gitFileTree :: [St.FileStatus] -> FileTree
gitFileTree statuses = simplifyFileTree $ foldr (uncurry insertIntoFileTree) emptyFileTree $ extractGitStatus =<< statuses

renderParent :: Maybe Text -> [(Text, Text)]
renderParent Nothing = []
renderParent (Just parent) = [("data-parent", parent)]

renderSubTree :: Maybe Text -> FilePath -> FileTree -> Widget
renderSubTree parent sub ft@(FileTreeDir _) = do
  checkId <- newIdent
  [whamlet|
    <details open>
      <summary>
        <input type=checkbox ##{checkId} .gitSelect *{renderParent parent}>
        <tt>#{sub}
      ^{renderFileTree (Just checkId) ft}
  |]
renderSubTree parent sub (FileTreeFile path status) = do
  checkId <- newIdent
  [whamlet|
    <input type=checkbox ##{checkId} .gitSelect .gitFile data-path=#{path} data-status=#{st} *{renderParent parent}>
    <tt *{sym}>#{sub}
  |]
  where
    sym :: [(Text, Text)]
    sym = case status of
      GitNewFile -> [("style", "color:var(--base0B);")]
      GitDeletedFile -> [("style", "color:var(--base08);")]
      GitChangedFile -> []
    st :: Text
    st = case status of
      GitNewFile -> "a"
      GitChangedFile -> "a"
      GitDeletedFile -> "r"

-- Css taken from: https://iamkate.com/code/tree-views/
fileTreeCss :: (Base16Index -> Text) -> routes -> Css
fileTreeCss base =
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
      background: var(--base0D) url('data:image/svg+xml;base64,#{encodeBase64 svg}') 0 0
    details[open] > summary::before
      background-position: calc(-2 * var(--radius)) 0
|]
  where
    svg :: Text
    svg =
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
      \<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"32\" height=\"16\">\
      \  <g fill=\""
        <> base Base00
        <> "\" transform=\"scale(0.8)\">\
           \    <path d=\"m5 9h4v-4h2v4h4v2h-4v4h-2v-4h-4z\"/>\
           \    <path d=\"m25 9h10v2h-10z\"/>\
           \  </g>\
           \</svg>"

fileTreeJs :: JavascriptUrl url
fileTreeJs =
  [julius|
  document.querySelectorAll(".gitSelect").forEach((checkbox) => {
    checkbox.addEventListener('change', (event) => {
      const id = checkbox.id
      var childs = Array.from(document.querySelectorAll(`.gitSelect[data-parent="${id}"]`))
      while(childs.length > 0) {
        const child = childs.pop()
        child.indeterminate = false
        child.checked = checkbox.checked
        const granchilds = document.querySelectorAll(`.gitSelect[data-parent="${child.id}"]`)
        childs.push(...granchilds)
      }

      var parentId = checkbox.getAttribute("data-parent")
      while(parentId) {
        var parent = document.getElementById(parentId)
        const childs = Array.from(document.querySelectorAll(`.gitSelect[data-parent="${parentId}"]`))
        const all = childs.every((child) => child.checked)
        const none = childs.every((child) => !child.checked)
        const inde = childs.some((child) => child.indeterminate)
        if(all && !inde) {
          parent.indeterminate = false
          parent.checked = true
        } else if(none && !inde) {
          parent.indeterminate = false
          parent.checked = false
        } else {
          parent.indeterminate = true
        }
        parentId = parent.getAttribute("data-parent")
      }
    })
  })
|]

renderFileTree :: Maybe Text -> FileTree -> Widget
renderFileTree _ (FileTreeFile _ _) = mempty
renderFileTree parent (FileTreeDir subs) = do
  [whamlet|
    <ul *{attrs}>
      $forall (path,sub) <- M.toList subs
        <li>
          ^{renderSubTree parent path sub}
  |]
  where
    attrs :: [(Text, Text)]
    attrs = [("class", "gitTree") | isNothing parent]

commitJs :: Text -> JavascriptUrl url
commitJs aggregateId =
  [julius|
  function aggregate_files() {
    var aggregate = document.getElementById(#{aggregateId})
    const paths = Array.from(document.querySelectorAll('.gitFile'))
      .filter((file) => file.checked)
      .map((file) => `${file.getAttribute("data-status")}:${file.getAttribute("data-path")}`)
      .join('\0')
    aggregate.value = paths
  }
|]

getGitR :: Handler Html
getGitR = do
  statuses <- St.gitStatusKorr
  let ft = gitFileTree statuses
  base <- getBase
  commitId <- newIdent
  msgId <- newIdent
  aggregateId <- newIdent
  logWrap $
    defaultLayout $ do
      toWidget $ fileTreeCss base
      toWidget fileTreeJs
      toWidget $ commitJs aggregateId
      Rcs.formsStyle
      [whamlet|
  <h1>Status
  ^{renderFileTree Nothing ft}
  <form onsubmit="aggregate_files()" action=@{GitR} method=POST>
    <input type="text" name=commitmsg ##{msgId}>
    <input type="hidden" name=commitfiles ##{aggregateId}>
    <button type="submit" ##{commitId}>
      Commit
  |]

data CommitData = CommitData
  { _ciMsg :: Text,
    _ciFiles :: Text -- \n separated filepaths from korrvigs root
  }
  deriving (Eq, Show)

makeLenses ''CommitData

extractFile :: Text -> (FilePath, Ci.CommitStatus)
extractFile file =
  let (st, path) = T.splitAt 2 file
   in if st == "a:"
        then (T.unpack path, Ci.CiAdded)
        else (T.unpack path, Ci.CiDeleted)

postGitR :: Handler Html
postGitR = do
  ci <-
    runInputPost $
      CommitData
        <$> ireq textField "commitmsg"
        <*> ireq textField "commitfiles"
  let files = T.split (== '\0') $ ci ^. ciFiles
  let cid = Ci.CiData (ci ^. ciMsg) (extractFile <$> files)
  Ci.gitCommitKorr cid
  redirect GitR
