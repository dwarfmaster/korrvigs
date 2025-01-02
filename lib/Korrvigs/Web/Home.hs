module Korrvigs.Web.Home (getHomeR, postHomeR) where

import Control.Lens
import Data.Text (Text)
import qualified Korrvigs.Note.New as NNote
import Korrvigs.Web.Backend
import Korrvigs.Web.Login (logWrap)
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Yesod
import Prelude hiding (not)

newtype NewNote = NewNote {_nnoteTitle :: Text}

makeLenses ''NewNote

newNoteForm :: Html -> MForm Handler (FormResult NewNote, Widget)
newNoteForm = renderDivs $ NewNote <$> areq textField "Title" Nothing

getHomeR :: Handler Html
getHomeR = do
  (nnoteW, nnoteEnctype) <- generateFormPost newNoteForm
  logWrap $
    defaultLayout $ do
      Rcs.formsStyle
      [whamlet|
      <h1>Welcome to Korrvigs
      <details .search-group>
        <summary>Create new note
        <form method=post action=@{HomeR} enctype=#{nnoteEnctype}>
          ^{nnoteW}
          <button>New note
    |]

postHomeR :: Handler Html
postHomeR = runNewNote

runNewNote :: Handler Html
runNewNote = do
  ((result, _), _) <- runFormPost newNoteForm
  case result of
    FormSuccess nnote | nnote ^. nnoteTitle /= "" -> do
      let settings = NNote.NewNote (nnote ^. nnoteTitle) Nothing
      i <- NNote.new settings
      redirect $ EntryR $ WId i
    _ -> getHomeR
