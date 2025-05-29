module Korrvigs.Web.Actions.New where

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Actions.Collections
import Korrvigs.Entry
import Korrvigs.Entry.New
import qualified Korrvigs.File.New as NFile
import qualified Korrvigs.Link.New as NLink
import qualified Korrvigs.Metadata.Media.New as NMedia
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Note.AST
import qualified Korrvigs.Note.New as NNote
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import System.FilePath
import System.IO.Temp
import Yesod hiding (joinPath)

newtype NewNote = NewNote {_nnoteTitle :: Text}

data NewLink = NewLink {_nlinkTitle :: Maybe Text, _nlinkUrl :: Text}

data NewFile = NewFile {_nfileTitle :: Maybe Text, _nfileContent :: FileInfo}

data NewMedia = NewMedia {_nmedInput :: Text, _nmedType :: Maybe MediaType}

makeLenses ''NewNote
makeLenses ''NewLink
makeLenses ''NewFile
makeLenses ''NewMedia

newTarget :: ActionTarget -> Bool
newTarget (TargetEntry _) = True
newTarget TargetHome = True
newTarget (TargetSearch _ _) = False
newTarget (TargetNoteCollection _ _) = True

mkNewTitle :: Text -> ActionTarget -> Text
mkNewTitle suffix TargetHome = "Create " <> suffix
mkNewTitle suffix _ = "Attach " <> suffix

extractParent :: ActionTarget -> Maybe Id
extractParent (TargetEntry entry) = Just $ entry ^. name
extractParent _ = Nothing

mkReaction :: ActionTarget -> Text -> Id -> Handler ActionReaction
mkReaction TargetHome _ i = do
  render <- getUrlRender
  pure $ def & reactRedirect ?~ render (EntryR $ WId i)
mkReaction (TargetEntry _) suffix i = do
  let htmlUrl =
        [hamlet|
    <p>
      Created #{suffix}:
      <a href=@{EntryR $ WId i}>
        <code>
          #{unId i}
  |]
  html <- htmlUrl <$> getUrlRenderParams
  pure $
    def
      & reactMsg ?~ html
      & reactClipboard ?~ unId i
mkReaction (TargetSearch _ _) _ _ = pure def
mkReaction (TargetNoteCollection note col) suffix i = do
  void $ addToCollection (note ^. noteEntry . name) col (ColItemEntry i)
  render <- getUrlRenderParams
  let htmlUrl =
        [hamlet|
    <p>
      Created #{suffix}:
      <a href=@{EntryR $ WId i}>
        <code>
          #{unId i}
  |]
  pure $
    def
      & reactMsg ?~ htmlUrl render
      & reactClipboard ?~ unId i

newNoteForm :: AForm Handler NewNote
newNoteForm = NewNote <$> areq textField "Title" Nothing

newNoteTitle :: ActionTarget -> Text
newNoteTitle = mkNewTitle "new note"

runNewNote :: NewNote -> ActionTarget -> Handler ActionReaction
runNewNote nnote tgt = do
  let settings =
        NNote.NewNote
          ( def
              & neParents .~ maybeToList (extractParent tgt)
          )
          (nnote ^. nnoteTitle)
  i <- NNote.new settings
  mkReaction tgt "new note" i

newLinkForm :: AForm Handler NewLink
newLinkForm =
  NewLink
    <$> aopt textField "Title" Nothing
    <*> areq textField "URL" Nothing

newLinkTitle :: ActionTarget -> Text
newLinkTitle = mkNewTitle "new link"

runNewLink :: NewLink -> ActionTarget -> Handler ActionReaction
runNewLink nlink tgt = do
  let settings =
        def
          & NLink.nlOffline .~ False
          & NLink.nlEntry . neTitle .~ nlink ^. nlinkTitle
          & NLink.nlEntry . neParents .~ maybeToList (extractParent tgt)
  i <- NLink.new (nlink ^. nlinkUrl) settings
  mkReaction tgt "new link" i

newFileForm :: AForm Handler NewFile
newFileForm =
  NewFile
    <$> aopt textField "Title" Nothing
    <*> fileAFormReq ("" {fsLabel = "File"})

newFileTitle :: ActionTarget -> Text
newFileTitle = mkNewTitle "new file"

runNewFile :: NewFile -> ActionTarget -> Handler ActionReaction
runNewFile nfile tgt =
  withRunInIO $ \runIO ->
    withSystemTempDirectory "korrUpload" $ \dir -> do
      let filename = T.unpack $ fileName $ nfile ^. nfileContent
      let path = joinPath [dir, filename]
      fileMove (nfile ^. nfileContent) path
      let settings =
            flip NFile.NewFile False $
              def
                & neTitle .~ nfile ^. nfileTitle
                & neParents .~ maybeToList (extractParent tgt)
      runIO $ do
        i <- NFile.new path settings
        mkReaction tgt "new file" i

newMediaForm :: AForm Handler NewMedia
newMediaForm =
  NewMedia . unTextarea
    <$> areq textareaField "Input" Nothing
    <*> areq (selectFieldList types) "Type" Nothing
  where
    types :: [(Text, Maybe MediaType)]
    types = ("Auto", Nothing) : ((displayMediaType &&& Just) <$> [minBound .. maxBound])

newMediaTitle :: ActionTarget -> Text
newMediaTitle = mkNewTitle "new media"

runNewMedia :: NewMedia -> ActionTarget -> Handler ActionReaction
runNewMedia nmedia tgt = do
  let nmed =
        NMedia.NewMedia
          { NMedia._nmEntry =
              def
                & neParents %~ maybe id (:) (extractParent tgt),
            NMedia._nmInput = nmedia ^. nmedInput,
            NMedia._nmType = nmedia ^. nmedType,
            NMedia._nmCapture = True
          }
  i <- NMedia.new nmed
  mkReaction tgt "new media" i
