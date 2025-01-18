module Korrvigs.Web.Home (getHomeR, postHomeR, newForms, runNewForms) where

import Conduit
import Control.Lens
import Control.Monad
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry.Ident
import Korrvigs.Entry.New
import Korrvigs.Favourites
import qualified Korrvigs.File.New as NFile
import qualified Korrvigs.Link.New as NLink
import qualified Korrvigs.Note.New as NNote
import Korrvigs.Utils (firstJustM)
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Widgets as Widgets
import System.FilePath
import System.IO.Temp
import Yesod hiding (joinPath)

newtype NewNote = NewNote {_nnoteTitle :: Text}

data NewLink = NewLink {_nlinkTitle :: Maybe Text, _nlinkUrl :: Text}

data NewFile = NewFile {_nfileTitle :: Maybe Text, _nfileContent :: FileInfo}

makeLenses ''NewNote
makeLenses ''NewLink
makeLenses ''NewFile

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
  pure
    [whamlet|
    $if not (null errMsgs)
      <ul>
        $forall msg <- errMsgs
          <li> #{msg}
    ^{newNote}
    ^{newLink}
    ^{newFile}
  |]

displayFavTree :: Int -> Text -> FavouriteTree -> Handler Widget
displayFavTree lvl cat favs = do
  let entries = favs ^. favEntries
  subs <- forM (M.toList $ favs ^. favSubs) $ uncurry (displayFavTree $ lvl + 1)
  let content =
        [whamlet|
    <ul>
      $forall (i,title) <- entries
        <li>
          <a href=@{EntryR (WId i)}>
            $maybe t <- title
              #{t}
            $nothing
              @#{unId i}
    $forall sub <- subs
      ^{sub}
  |]
  pure $ Widgets.mkSection lvl [("class", "collapsed") | lvl > 1] [] (header lvl) content
  where
    header :: Int -> Widget
    header 1 = [whamlet|<h2> ^{Widgets.headerSymbol "★"} #{cat}|]
    header 2 = [whamlet|<h3> ^{Widgets.headerSymbol "★"} #{cat}|]
    header 3 = [whamlet|<h4> ^{Widgets.headerSymbol "★"} #{cat}|]
    header 4 = [whamlet|<h5> ^{Widgets.headerSymbol "★"} #{cat}|]
    header _ = [whamlet|<h6> ^{Widgets.headerSymbol "★"} #{cat}|]

displayHome :: [Text] -> Handler Html
displayHome errMsgs = do
  nw <- newForms HomeR "Create" errMsgs
  let nwHd = [whamlet|<h2> ^{Widgets.headerSymbol "⊕"} Create entry|]
  favs <- displayFavTree 1 "Favourites" =<< favTree
  defaultLayout $ do
    Rcs.entryStyle
    Rcs.formsStyle
    Widgets.sectionLogic
    [whamlet|
    <h1>Welcome to Korrvigs
    ^{Widgets.mkSection 1 [] [] nwHd nw}
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
      runForm parent newFileForm runNewFile
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
  let settings = NNote.NewNote (nnote ^. nnoteTitle) parent
   in NNote.new settings

runNewLink :: Maybe Id -> NewLink -> Handler Id
runNewLink parent nlink =
  let settings =
        NLink.NewLink
          { NLink._nlOffline = False,
            NLink._nlDate = Nothing,
            NLink._nlTitle = nlink ^. nlinkTitle,
            NLink._nlParent = parent
          }
   in NLink.new (nlink ^. nlinkUrl) settings

runNewFile :: Maybe Id -> NewFile -> Handler Id
runNewFile parent nfile =
  withRunInIO $ \runIO ->
    withSystemTempDirectory "korrUpload" $ \dir -> do
      let filename = T.unpack $ fileName $ nfile ^. nfileContent
      let path = joinPath [dir, filename]
      fileMove (nfile ^. nfileContent) path
      let settings =
            NFile.NewFile $
              def
                & neTitle .~ nfile ^. nfileTitle
                & neParents .~ maybeToList parent
      runIO $ NFile.new path settings
