module Korrvigs.Web.Actions.Compute where

import Control.Lens
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Monad.Computation
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Opaleye
import Yesod

computeTarget :: ActionTarget -> ActionCond
computeTarget (TargetEntry entry)
  | entry ^. kind == Note || entry ^. kind == File =
      ActCondAlways
computeTarget (TargetNoteCode _ _) = ActCondAlways
computeTarget _ = ActCondNever

computeForm :: ActionTarget -> Handler (AForm Handler (Text, Bool))
computeForm (TargetEntry entry) = do
  comps <- rSelect $ do
    comp <- selectTable computationsTable
    where_ $ comp ^. sqlCompEntry .== sqlInt4 (entry ^. entryId)
    pure $ comp ^. sqlCompName
  let opts = mkOpt <$> zip [1 ..] comps
  let field = selectField $ pure $ mkOptionList opts
  pure $
    (,)
      <$> areq field "Computation" Nothing
      <*> areq checkBoxField "Force" (Just False)
  where
    mkOpt :: (Int, Text) -> Option Text
    mkOpt (i, val) = Option val val $ T.pack $ show i
computeForm (TargetNoteCode _ code) =
  pure $ (code,) <$> areq checkBoxField "Force" (Just False)
computeForm _ = pure $ pure ("", False)

computeTitle :: ActionTarget -> Text
computeTitle = const "Compute"

runCompute :: (Text, Bool) -> ActionTarget -> Handler ActionReaction
runCompute (comp, force) (TargetEntry entry) =
  doRunCompute (entry ^. entryName) comp force
runCompute (comp, force) (TargetNoteCode note _) =
  doRunCompute (note ^. noteEntry . entryName) comp force
runCompute _ _ = pure def

doRunCompute :: Id -> Text -> Bool -> Handler ActionReaction
doRunCompute i cmp force = do
  render <- getUrlRenderParams
  getComputation i cmp >>= \case
    Nothing -> pure $ def & reactMsg ?~ [hamlet|<p>Failed to load computation.|] render
    Just comp ->
      runner comp >>= \case
        Left err ->
          pure $ def & reactMsg ?~ [hamlet|<p>Computation failed with: #{err}|] render
        Right _ -> pure $ def & reactMsg ?~ [hamlet|<p>Success !|] render
  where
    runner = if force then runForce else runLazy
