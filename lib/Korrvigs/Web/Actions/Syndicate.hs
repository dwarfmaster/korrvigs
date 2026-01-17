module Korrvigs.Web.Actions.Syndicate where

import Control.Lens
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad.Sync
import Korrvigs.Syndicate.JSON
import qualified Korrvigs.Syndicate.Sync as Sync
import Korrvigs.Web.Actions.Defs
import qualified Korrvigs.Web.Actions.New as New
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

synUpdateTarget :: ActionTarget -> ActionCond
synUpdateTarget (TargetEntry entry) | entry ^. kind == Syndicate = ActCondAlways
synUpdateTarget _ = ActCondNever

synUpdateForm :: ActionTarget -> AForm Handler (Maybe Text, Maybe Text)
synUpdateForm (TargetEntry entry) = case entry ^. entryKindData of
  SyndicateD syn ->
    (,)
      <$> aopt textField "URL" ((Just . Just) =<< syn ^. synUrl)
      <*> aopt textField "Filter" (renderFilters $ syn ^. synFilters)
  _ -> pure def
  where
    renderFilters :: [(Id, Text)] -> Maybe (Maybe Text)
    renderFilters [] = Nothing
    renderFilters flts =
      Just $ Just $ T.intercalate "," $ (\(i, code) -> unId i <> "#" <> code) <$> flts
synUpdateForm _ = pure def

synUpdateTitle :: ActionTarget -> Text
synUpdateTitle = const "Update syndicate information"

runSynUpdate :: (Maybe Text, Maybe Text) -> ActionTarget -> Handler ActionReaction
runSynUpdate (url, filtersTxt) (TargetEntry entry) =
  case entry ^. entryKindData of
    SyndicateD syn -> do
      let filters = New.parseSyndicateFilter filtersTxt
      Sync.updateImpl syn $ pure . (synjsUrl .~ url) . (synjsFilters .~ filters)
      syncFileOfKind (syn ^. synPath) Syndicate
      render <- getUrlRenderParams
      pure $
        def
          & reactMsg ?~ [hamlet|<p>Update successful !|] render
          & reactRedirect ?~ render (EntryR $ WId $ entry ^. entryName) []
    _ -> pure def
runSynUpdate _ _ = pure def
