module Korrvigs.Web.Actions.EventSync (syncEvTarget, syncEvForm, runSyncEv, syncEvTitle) where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Korrvigs.Calendar (listCalendars)
import qualified Korrvigs.Calendar.DAV as DAV
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

syncEvTarget :: ActionTarget -> Bool
syncEvTarget TargetHome = True
syncEvTarget (TargetEntry entry) | entry ^. kind == Calendar = True
syncEvTarget _ = False

syncEvForm :: AForm Handler Text
syncEvForm = pure undefined

syncEvTitle :: ActionTarget -> Text
syncEvTitle = const "Sync Events"

mkMsg :: [Calendar] -> (Route WebData -> [(Text, Text)] -> Text) -> Html
mkMsg cals =
  [hamlet|
  <p> Synced calendar(s):
  <ul>
    $forall cal <- cals
      <li>
        <a href=@{EntryR $ WId $ view (calEntry . name) cal}>
          #{unId $ view (calEntry . name) cal}
|]

runSyncEv :: Text -> ActionTarget -> Handler ActionReaction
runSyncEv pwd TargetHome = do
  cals <- listCalendars
  DAV.sync True cals pwd
  render <- getUrlRenderParams
  pure $ def & reactMsg ?~ mkMsg cals render
runSyncEv pwd (TargetEntry entry) = case entry ^. kindData of
  CalendarD cal -> do
    DAV.sync True [cal] pwd
    render <- getUrlRenderParams
    pure $ def & reactMsg ?~ mkMsg [cal] render
  _ -> pure def
runSyncEv _ _ = pure def
