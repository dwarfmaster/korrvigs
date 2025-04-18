module Korrvigs.Web.Home (getHomeR, postHomeR, newForms, runNewForms) where

import Conduit
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry.Ident
import Korrvigs.Entry.New
import qualified Korrvigs.File.New as NFile
import qualified Korrvigs.Link.New as NLink
import Korrvigs.Metadata.Collections
import qualified Korrvigs.Metadata.Media.New as NMedia
import Korrvigs.Metadata.Media.Ontology (MediaType, displayMediaType)
import qualified Korrvigs.Note.New as NNote
import Korrvigs.Utils (firstJustM)
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Collections as Cols
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Widgets
import System.FilePath
import System.IO.Temp
import Text.Blaze (textValue, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Yesod hiding (joinPath)

newtype NewNote = NewNote {_nnoteTitle :: Text}

data NewLink = NewLink {_nlinkTitle :: Maybe Text, _nlinkUrl :: Text}

data NewFile = NewFile {_nfileTitle :: Maybe Text, _nfileContent :: FileInfo}

data NewMedia = NewMedia {_nmedInput :: Text, _nmedType :: Maybe MediaType}

makeLenses ''NewNote
makeLenses ''NewLink
makeLenses ''NewFile
makeLenses ''NewMedia

newNoteForm :: Html -> MForm Handler (FormResult NewNote, Widget)
newNoteForm = identifyForm "newnote" $ renderDivs $ NewNote <$> areq textField "Title" Nothing

newLinkForm :: Html -> MForm Handler (FormResult NewLink, Widget)
newLinkForm =
  identifyForm "newlink" $
    renderDivs $
      NewLink
        <$> aopt textField "Title" Nothing
        <*> areq textField "URL" Nothing

newFileForm :: Html -> MForm Handler (FormResult NewFile, Widget)
newFileForm =
  identifyForm "newfile" $
    renderDivs $
      NewFile
        <$> aopt textField "Title" Nothing
        <*> fileAFormReq ("" {fsLabel = "File"})

newMediaForm :: Html -> MForm Handler (FormResult NewMedia, Widget)
newMediaForm =
  identifyForm "newmedia" $
    renderDivs $
      NewMedia
        <$> areq textField "Input" Nothing
        <*> areq (selectFieldList types) "Type" Nothing
  where
    types :: [(Text, Maybe MediaType)]
    types = ("Auto", Nothing) : ((displayMediaType &&& Just) <$> [minBound .. maxBound])

renderForm :: Route WebData -> Text -> (Html -> MForm Handler (FormResult a, Widget)) -> Handler Widget
renderForm postUrl title form = do
  (widget, enctype) <- generateFormPost form
  pure
    [whamlet|
    <details .search-group>
      <summary>
        #{title}
      <form method=post action=@{postUrl} enctype=#{enctype}>
        ^{widget}
        <button>
          #{title}
  |]

newForms :: Route WebData -> Text -> [Text] -> Handler Widget
newForms postUrl prefix errMsgs = do
  newNote <- renderForm postUrl (prefix <> " new note") newNoteForm
  newLink <- renderForm postUrl (prefix <> " new link") newLinkForm
  newFile <- renderForm postUrl (prefix <> " new file") newFileForm
  newMedia <- renderForm postUrl (prefix <> " new media") newMediaForm
  pure
    [whamlet|
    $if not (null errMsgs)
      <ul>
        $forall msg <- errMsgs
          <li> #{msg}
    ^{newNote}
    ^{newLink}
    ^{newFile}
    ^{newMedia}
  |]

displayHome :: [Text] -> Handler Html
displayHome errMsgs = do
  nw <- newForms HomeR "Create" errMsgs
  let nwHd = [whamlet|<h2> ^{Widgets.headerSymbol "⊕"} Create entry|]
  render <- getUrlRender
  let hd = Html.a "Favourites" ! Attr.href (textValue $ render $ ColR ["Favourite"])
  favs <- Cols.displayTree Nothing 1 1 hd ["Favourite"] =<< colTree MiscCollection ["Favourite"] True
  defaultLayout $ do
    setTitle "Korrvigs's Home"
    setDescriptionIdemp "Korrvigs home page"
    Rcs.entryStyle
    Rcs.formsStyle
    Widgets.sectionLogic
    [whamlet|
    <h1>Welcome to Korrvigs
    ^{void $ Widgets.mkSection 1 [] [] nwHd nw}
    ^{favs}
  |]

getHomeR :: Handler Html
getHomeR = displayHome []

postHomeR :: Handler Html
postHomeR =
  runNewForms Nothing >>= \case
    Nothing -> displayHome []
    Just (Left err) -> displayHome err
    Just (Right i) -> redirect $ EntryR $ WId i

-- Parse the results of new queries, with possibility of specifying a parent. If there
-- was an error, return it.
runNewForms :: Maybe Id -> Handler (Maybe (Either [Text] Id))
runNewForms parent =
  firstJustM
    [ runForm parent newNoteForm runNewNote,
      runForm parent newLinkForm runNewLink,
      runForm parent newFileForm runNewFile,
      runForm parent newMediaForm runNewMedia
    ]

runForm ::
  Maybe Id ->
  (Html -> MForm Handler (FormResult a, Widget)) ->
  (Maybe Id -> a -> Handler Id) ->
  Handler (Maybe (Either [Text] Id))
runForm parent form handler = do
  ((result, _), _) <- runFormPost form
  case result of
    FormSuccess x -> Just . Right <$> handler parent x
    FormMissing -> pure Nothing
    FormFailure err -> pure $ Just $ Left err

runNewNote :: Maybe Id -> NewNote -> Handler Id
runNewNote parent nnote =
  let settings =
        NNote.NewNote
          (def & neParents .~ maybeToList parent)
          (nnote ^. nnoteTitle)
   in NNote.new settings

runNewLink :: Maybe Id -> NewLink -> Handler Id
runNewLink parent nlink =
  let settings =
        def
          & NLink.nlOffline .~ False
          & NLink.nlEntry . neTitle .~ nlink ^. nlinkTitle
          & NLink.nlEntry . neParents .~ maybeToList parent
   in NLink.new (nlink ^. nlinkUrl) settings

runNewFile :: Maybe Id -> NewFile -> Handler Id
runNewFile parent nfile =
  withRunInIO $ \runIO ->
    withSystemTempDirectory "korrUpload" $ \dir -> do
      let filename = T.unpack $ fileName $ nfile ^. nfileContent
      let path = joinPath [dir, filename]
      fileMove (nfile ^. nfileContent) path
      let settings =
            flip NFile.NewFile False $
              def
                & neTitle .~ nfile ^. nfileTitle
                & neParents .~ maybeToList parent
      runIO $ NFile.new path settings

runNewMedia :: Maybe Id -> NewMedia -> Handler Id
runNewMedia parent nmedia =
  let nmed =
        NMedia.NewMedia
          { NMedia._nmEntry = def & neParents %~ maybe id (:) parent,
            NMedia._nmInput = nmedia ^. nmedInput,
            NMedia._nmType = nmedia ^. nmedType
          }
   in NMedia.new nmed
