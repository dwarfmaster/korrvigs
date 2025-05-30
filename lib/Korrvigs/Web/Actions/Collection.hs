module Korrvigs.Web.Actions.Collection where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Monad.Collections
import Korrvigs.Note.AST
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

colTarget :: ActionTarget -> Bool
colTarget (TargetEntry _) = True
colTarget (TargetSearch _ _) = True
colTarget (TargetNoteCollection _ _) = True
colTarget _ = False

colForm :: AForm Handler (Id, Text)
colForm =
  (,) . MkId
    <$> areq textField "Entry" Nothing
    <*> areq textField "Collection" Nothing

colTitle :: ActionTarget -> Text
colTitle = const "Add to collection"

mkReaction :: Bool -> Id -> Text -> Handler Html
mkReaction success i col = htmlUrl <$> getUrlRenderParams
  where
    htmlUrl =
      [hamlet|
      <p>
        $if success
          Added to collection
        $else
          Failed to add to collection
        <a href=@{NoteColR (WId i) col}>
          #{unId i}##{col}
    |]

runCol :: (Id, Text) -> ActionTarget -> Handler ActionReaction
runCol (i, col) (TargetEntry entry) = do
  success <- addToCollection i col $ ColItemEntry $ entry ^. name
  html <- mkReaction success i col
  pure $ def & reactMsg ?~ html
runCol (i, col) (TargetSearch q _) = do
  success <- addToCollection i col $ ColItemQuery q
  html <- mkReaction success i col
  pure $ def & reactMsg ?~ html
runCol (i, col) (TargetNoteCollection noteId noteCol) = do
  success <- addToCollection i col $ ColItemInclude (noteId ^. noteEntry . name) noteCol
  html <- mkReaction success i col
  pure $ def & reactMsg ?~ html
runCol _ _ = pure def
