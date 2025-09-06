module Korrvigs.Web.Actions.Remove where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Korrvigs.Monad.Remove
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Yesod

removeTarget :: ActionTarget -> Bool
removeTarget (TargetEntry _) = True
removeTarget _ = False

removeForm :: AForm Handler Bool
removeForm = areq checkBoxField "Delete ?" (Just False)

removeTitle :: ActionTarget -> Text
removeTitle = const "Remove"

runRemove :: Bool -> ActionTarget -> Handler ActionReaction
runRemove False _ = pure $ def & reactMsg ?~ [shamlet|<p>Check the box to remove entry|]
runRemove True (TargetEntry entry) = do
  removeDWIM entry
  render <- getUrlRender
  pure $ def & reactRedirect ?~ render HomeR
runRemove True _ = pure def
