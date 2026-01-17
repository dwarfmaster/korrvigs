module Korrvigs.Web.Actions.Metadata where

import Control.Lens
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Monad.Metadata
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

mtdtHookTarget :: ActionTarget -> ActionCond
mtdtHookTarget (TargetEntry _) = ActCondAlways
mtdtHookTarget _ = ActCondNever

mtdtHookForm :: AForm Handler MetadataHook
mtdtHookForm = areq field "Metadata hook" Nothing
  where
    field :: Field Handler MetadataHook
    field = selectField $ pure $ mkOptionList $ mkOpt <$> zip [1 ..] [minBound .. maxBound]
    mkOpt :: (Int, MetadataHook) -> Option MetadataHook
    mkOpt (i, hook) = Option (displayHook hook) hook $ T.pack $ show i
    displayHook :: MetadataHook -> Text
    displayHook HookAggregate = "Aggregate"

mtdtHookTitle :: ActionTarget -> Text
mtdtHookTitle = const "Run hook"

runMtdtHook :: MetadataHook -> ActionTarget -> Handler ActionReaction
runMtdtHook hook (TargetEntry entry) = do
  runHook entry hook
  render <- getUrlRenderParams
  pure $
    def
      & reactMsg ?~ [hamlet|<p>Hook run !|] render
      & reactRedirect ?~ render (EntryR $ WId $ entry ^. entryName) []
runMtdtHook _ _ = pure def
