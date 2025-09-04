module Korrvigs.Web.Actions.Bibtex where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Note.AST
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Search.Form as Search
import Yesod

bibtexTarget :: ActionTarget -> Bool
bibtexTarget (TargetEntry _) = True
bibtexTarget TargetHome = False
bibtexTarget (TargetSearch _ _) = True
bibtexTarget (TargetNoteCollection _ _) = True
bibtexTarget (TargetNoteSub _ _) = False
bibtexTarget (TargetNoteCode _ _) = False

bibtexForm :: AForm Handler ()
bibtexForm = pure ()

bibtexTitle :: ActionTarget -> Text
bibtexTitle = const "Export BibTeX"

runBibtex :: () -> ActionTarget -> Handler ActionReaction
runBibtex () (TargetEntry entry) = do
  render <- getUrlRender
  let url = EntryBibtexR $ WId $ entry ^. entryName
  pure $ def & reactRedirect ?~ render url
runBibtex () TargetHome = pure def
runBibtex () (TargetSearch q _) = do
  render <- getUrlRenderParams
  let params = Search.getParameters Nothing q ColList
  pure $ def & reactRedirect ?~ render SearchBibtexR params
runBibtex () (TargetNoteCollection note col) = do
  render <- getUrlRender
  let url = NoteColBibtexR (WId $ note ^. noteEntry . entryName) col
  pure $ def & reactRedirect ?~ render url
runBibtex () (TargetNoteSub _ _) = pure def
runBibtex () (TargetNoteCode _ _) = pure def
