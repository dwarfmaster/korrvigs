module Korrvigs.Web.Home (getHomeR, postHomeR) where

import Control.Lens
import Data.Text (Text)
import qualified Korrvigs.Link.New as NLink
import qualified Korrvigs.Note.New as NNote
import Korrvigs.Web.Backend
import Korrvigs.Web.Login (logWrap)
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Yesod
import Prelude hiding (not)

newtype NewNote = NewNote {_nnoteTitle :: Text}

data NewLink = NewLink {_nlinkTitle :: Maybe Text, _nlinkUrl :: Text}

makeLenses ''NewNote
makeLenses ''NewLink

newNoteForm :: Html -> MForm Handler (FormResult NewNote, Widget)
newNoteForm = renderDivs $ NewNote <$> areq textField "Title" Nothing

newLinkForm :: Html -> MForm Handler (FormResult NewLink, Widget)
newLinkForm =
  renderDivs $
    NewLink
      <$> aopt textField "Title" Nothing
      <*> areq textField "URL" Nothing

renderForm :: Text -> (Html -> MForm Handler (FormResult a, Widget)) -> Handler Widget
renderForm title form = do
  (widget, enctype) <- generateFormPost form
  pure
    [whamlet|
    <details .search-group>
      <summary>
        #{title}
      <form method=post action=@{HomeR} enctype=#{enctype}>
        ^{widget}
        <button>
          #{title}
  |]

getHomeR :: Handler Html
getHomeR = do
  newNote <- renderForm "Create new note" newNoteForm
  newLink <- renderForm "Create new link" newLinkForm
  logWrap $
    defaultLayout $ do
      Rcs.formsStyle
      [whamlet|
      <h1>Welcome to Korrvigs
      ^{newNote}
      ^{newLink}
    |]

postHomeR :: Handler Html
postHomeR = do
  runNewNote
  runNewLink
  getHomeR

runNewNote :: Handler ()
runNewNote = do
  ((result, _), _) <- runFormPost newNoteForm
  case result of
    FormSuccess nnote | nnote ^. nnoteTitle /= "" -> do
      let settings = NNote.NewNote (nnote ^. nnoteTitle) Nothing
      i <- NNote.new settings
      redirect $ EntryR $ WId i
    _ -> pure ()

runNewLink :: Handler ()
runNewLink = do
  ((result, _), _) <- runFormPost newLinkForm
  case result of
    FormSuccess nlink | nlink ^. nlinkUrl /= "" -> do
      let settings =
            NLink.NewLink
              { NLink._nlOffline = False,
                NLink._nlDate = Nothing,
                NLink._nlTitle = nlink ^. nlinkTitle,
                NLink._nlParent = Nothing
              }
      i <- NLink.new (nlink ^. nlinkUrl) settings
      redirect $ EntryR $ WId i
    _ -> pure ()
