module Korrvigs.Web.Actions.Parent where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Korrvigs.Actions.Metadata
import Korrvigs.Actions.SQL
import Korrvigs.Entry
import Korrvigs.Utils
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

parentTarget :: ActionTarget -> Bool
parentTarget (TargetEntry _) = True
parentTarget _ = False

parentForm :: AForm Handler Id
parentForm = MkId <$> areq textField "Parent" Nothing

parentAddTitle :: ActionTarget -> Text
parentAddTitle = const "Add parent"

parentRmTitle :: ActionTarget -> Text
parentRmTitle = const "Remove parent"

runParent :: Id -> [Id] -> [Id] -> Handler ActionReaction
runParent i toAdd toRm =
  load i >>= \case
    Nothing -> notFound
    Just entry ->
      findM (fmap null . load) (toAdd ++ toRm) >>= \case
        Nothing -> do
          updateParents entry toAdd toRm
          html <- htmlUrl <$> getUrlRenderParams
          pure $ def & reactMsg ?~ html
        Just parent ->
          pure $ def & reactMsg ?~ [shamlet|<p>Could not find parent: #{unId parent}|]
  where
    htmlUrl =
      [hamlet|
      $if not (null toAdd)
        <p>
          Added parents:
        <ul>
          $forall added <- toAdd
            <li>
              <a href=@{EntryR $ WId added}>
                #{"@" <> unId added}
      $if not (null toRm)
        <p>
          Removed parents:
        <ul>
          $forall rm <- toRm
            <li>
              <a href=@{EntryR $ WId rm}>
                #{"@" <> unId rm}
    |]

runParentAdd :: Id -> ActionTarget -> Handler ActionReaction
runParentAdd parent (TargetEntry i) = runParent i [parent] []
runParentAdd _ _ = pure def

runParentRm :: Id -> ActionTarget -> Handler ActionReaction
runParentRm parent (TargetEntry i) = runParent i [] [parent]
runParentRm _ _ = pure def
