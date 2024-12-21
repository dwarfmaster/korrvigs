module Korrvigs.Web.Git (getGitR, postGitR) where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Korrvigs.Utils.Git.Status as St
import Korrvigs.Web.Backend
import Korrvigs.Web.Login (logWrap)
import System.FilePath
import Yesod

data GitStatus
  = GitNewFile
  | GitDeletedFile
  | GitChangedFile

data FileTree
  = FileTreeDir (Map Text FileTree)
  | FileTreeFile FilePath GitStatus

emptyFileTree :: FileTree
emptyFileTree = FileTreeDir M.empty

singletonFileTree :: FilePath -> GitStatus -> [Text] -> FileTree
singletonFileTree path st [] = FileTreeFile path st
singletonFileTree path st (part : prts) = FileTreeDir $ M.singleton part $ singletonFileTree path st prts

insertIntoFileTree' :: FilePath -> GitStatus -> [Text] -> FileTree -> FileTree
insertIntoFileTree' _ _ _ (FileTreeFile _ _) = error "Invalid insertion into filetree : inserting structure onto already existing file"
insertIntoFileTree' _ _ [] _ = error "Invalid insertion into filetree : inserting file onto already existing structure"
insertIntoFileTree' path st (part : prts) (FileTreeDir subs) = case M.lookup part subs of
  Nothing -> FileTreeDir $ M.insert part (singletonFileTree path st prts) subs
  Just ft -> FileTreeDir $ M.insert part (insertIntoFileTree' path st prts ft) subs

insertIntoFileTree :: FilePath -> GitStatus -> FileTree -> FileTree
insertIntoFileTree path st = insertIntoFileTree' path st comps
  where
    comps :: [Text]
    comps = T.pack <$> splitDirectories path

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

gitFileTree :: [St.FileStatus] -> FileTree
gitFileTree statuses = foldr (uncurry insertIntoFileTree) emptyFileTree $ extractGitStatus =<< statuses

renderSubTree :: Text -> FileTree -> Widget
renderSubTree sub ft@(FileTreeDir _) =
  [whamlet|
    <tt>#{sub}
    ^{renderFileTree ft}
  |]
renderSubTree sub (FileTreeFile _ status) =
  [whamlet|
    #{sym}
    <tt>#{sub}
  |]
  where
    sym :: Text
    sym = case status of
      GitNewFile -> "⊕"
      GitDeletedFile -> "⊖"
      GitChangedFile -> ""

renderFileTree :: FileTree -> Widget
renderFileTree (FileTreeFile _ _) = mempty
renderFileTree (FileTreeDir subs) =
  [whamlet|
    <ul>
      $forall (path,sub) <- M.toList subs
        <li>
          ^{renderSubTree path sub}
  |]

getGitR :: Handler Html
getGitR = do
  statuses <- St.gitStatusKorr
  let ft = gitFileTree statuses
  logWrap $
    defaultLayout
      [whamlet|
  <h1>Status
  ^{renderFileTree ft}
  |]

postGitR :: Handler Html
postGitR = pure undefined
