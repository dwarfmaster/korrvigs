module Korrvigs.Web.Actions.EventSync (syncEvTarget, syncEvForm, runSyncEv, syncEvTitle) where

import Control.Lens
import Data.Default
import Data.IORef
import Data.Text (Text)
import qualified Korrvigs.Calendar.DAV as DAV
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Yesod

syncEvTarget :: ActionTarget -> Bool
syncEvTarget TargetHome = True
syncEvTarget _ = False

syncEvForm :: AForm Handler Text
syncEvForm = areq passwordField "dav-password" Nothing

syncEvTitle :: ActionTarget -> Text
syncEvTitle = const "Sync Events"

mkMsg :: Bool -> Text -> (Route WebData -> [(Text, Text)] -> Text) -> Html
mkMsg r msg =
  [hamlet|
  <p> #{status} 
  <pre>
    <code>
      #{msg}
|]
  where
    status :: Text
    status = if r then "Success" else "Failure"

runSyncEv :: Text -> ActionTarget -> Handler ActionReaction
runSyncEv pwd TargetHome = do
  logging <- liftIO $ newIORef ""
  r <- DAV.sync (\txt -> liftIO $ modifyIORef logging (<> "\n" <> txt)) pwd
  render <- getUrlRenderParams
  msg <- liftIO $ readIORef logging
  pure $ def & reactMsg ?~ mkMsg r msg render
runSyncEv _ _ = pure def
