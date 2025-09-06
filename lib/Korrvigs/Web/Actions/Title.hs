module Korrvigs.Web.Actions.Title where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad.Metadata
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

titleTarget :: ActionTarget -> Bool
titleTarget (TargetEntry _) = True
titleTarget _ = False

rmTitleTarget :: ActionTarget -> Bool
rmTitleTarget (TargetEntry entry) = entry ^. kind /= Note && isJust (entry ^. entryTitle)
rmTitleTarget _ = False

titleForm :: AForm Handler Text
titleForm = areq textField "Title" Nothing

rmTitleForm :: AForm Handler ()
rmTitleForm = pure ()

titleTitle :: ActionTarget -> Text
titleTitle = const "Set title"

rmTitleTitle :: ActionTarget -> Text
rmTitleTitle = const "Unset title"

runTitle :: Text -> ActionTarget -> Handler ActionReaction
runTitle ntitle (TargetEntry entry) = do
  let title = if T.null ntitle then Nothing else Just ntitle
  updateTitle entry title
  render <- getUrlRender
  pure $ def & reactRedirect ?~ render (EntryR $ WId $ entry ^. entryName)
runTitle _ _ = pure def

runRmTitle :: () -> ActionTarget -> Handler ActionReaction
runRmTitle () (TargetEntry entry) = do
  updateTitle entry Nothing
  render <- getUrlRender
  pure $ def & reactRedirect ?~ render (EntryR $ WId $ entry ^. entryName)
runRmTitle _ _ = pure def
