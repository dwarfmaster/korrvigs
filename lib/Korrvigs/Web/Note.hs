module Korrvigs.Web.Note
  ( getNoteR,
    postNoteR,
    getNoteSubR,
    postNoteSubR,
    getNoteColR,
    postNoteColR,
    getNoteNamedSubR,
    getNoteNamedCodeR,
  )
where

import Conduit
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit.Combinators (fold)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Monad.Sync (syncFileOfKind)
import Korrvigs.Note
import Korrvigs.Note.AST
import Korrvigs.Note.Pandoc
import Korrvigs.Web.Actions
import Korrvigs.Web.Backend
import Korrvigs.Web.Entry.Note (embedContent)
import qualified Korrvigs.Web.PhotoSwipe as PhotoSwipe
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Search
import Korrvigs.Web.Search.Form
import Korrvigs.Web.Search.Results
import System.IO
import Yesod hiding (check)

getNoteR :: WebId -> Handler LT.Text
getNoteR (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. kindData of
      NoteD note -> liftIO $ LEnc.decodeUtf8 <$> BSL.readFile (note ^. notePath)
      _ -> notFound

postNoteR :: WebId -> Handler LT.Text
postNoteR (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. kindData of
      NoteD note -> do
        body <- runConduit $ rawRequestBody .| fold
        let txt = Enc.decodeUtf8 body
        nt <- readNoteFromText parsePandoc txt
        case nt of
          Left err -> throwM $ KMiscError err
          Right doc -> do
            let path = note ^. notePath
            fd <- liftIO $ openFile path WriteMode
            writeNote fd doc >>= \case
              Just err -> do
                liftIO $ hClose fd
                throwM $ KMiscError err
              Nothing -> pure ()
            liftIO $ hClose fd
            syncFileOfKind path Note
            redirect $ NoteR $ WId i
      _ -> notFound

getNoteSubR :: WebId -> WebAnyLoc -> Handler LT.Text
getNoteSubR (WId i) (WLoc loc) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. kindData of
      NoteD note ->
        readNote (note ^. notePath) >>= \case
          Left err -> throwM $ KMiscError err
          Right doc -> case loc of
            LocCode lc -> case doc ^? code lc of
              Nothing -> notFound
              Just txt -> pure $ LT.fromStrict txt
            LocSub lc -> case doc ^? sub lc of
              Nothing -> notFound
              Just hd -> pure $ LEnc.decodeUtf8 $ writeHeaderLazy hd
            LocCheck lc -> case doc ^? check lc of
              Nothing -> notFound
              Just cb -> pure $ renderTaskStatus cb
            LocTask lc -> case doc ^? task lc . tskStatus of
              Nothing -> notFound
              Just tk -> pure $ renderTaskStatus tk
      _ -> notFound
  where
    renderTaskStatus :: TaskStatus -> LT.Text
    renderTaskStatus TaskTodo = "todo"
    renderTaskStatus TaskImportant = "important"
    renderTaskStatus TaskOngoing = "started"
    renderTaskStatus TaskBlocked = "blocked"
    renderTaskStatus TaskDone = "done"
    renderTaskStatus TaskDont = "dont"

postNoteSubR :: WebId -> WebAnyLoc -> Handler LT.Text
postNoteSubR (WId i) (WLoc loc) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. kindData of
      NoteD note ->
        readNote (note ^. notePath) >>= \case
          Left err -> throwM $ KMiscError err
          Right doc -> do
            body <- runConduit $ rawRequestBody .| fold
            let txt = Enc.decodeUtf8 body
            ndoc <- case loc of
              LocCode lc -> pure $ setCode lc doc txt
              LocSub lc -> do
                let lvl = length $ lc ^. subOffsets
                hd <- readNoteFromText (parseTopBlocks lvl) txt
                case hd of
                  Left err -> throwM $ KMiscError err
                  Right bks -> pure $ doc & subs lc .~ bks
              LocCheck lc -> do
                cb <- parseTaskStatus txt
                pure $ setCheck lc doc cb
              LocTask lc -> do
                tk <- parseTaskStatus txt
                pure $
                  doc
                    & task lc . tskStatus .~ tk
                    & task lc . tskStatusName .~ txt
            let path = note ^. notePath
            fd <- liftIO $ openFile path WriteMode
            writeNote fd ndoc >>= \case
              Just err -> do
                liftIO $ hClose fd
                throwM $ KMiscError err
              Nothing -> pure ()
            liftIO $ hClose fd
            syncFileOfKind path Note
            redirect $ NoteSubR (WId i) (WLoc loc)
      _ -> notFound
  where
    parseTaskStatus txt = maybe (throwM $ KMiscError $ "\"" <> txt <> "\" is not a valid task state") pure $ parseStatusName txt

getNoteColR :: WebId -> Text -> Handler TypedContent
getNoteColR (WId i) col = do
  accept <- lookupHeader "accept"
  if maybe False ("application/json" `BS.isPrefixOf`) accept
    then toTypedContent <$> getNoteJson i col
    else toTypedContent <$> (getNoteWidget i col >>= defaultLayout)

getNoteJson :: Id -> Text -> Handler Value
getNoteJson i col = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? kindData . _NoteD
  md <- readNote (note ^. notePath) >>= throwEither (\err -> KMiscError $ "Failed to load node " <> T.pack (note ^. notePath) <> ": " <> err)
  items <- maybe notFound pure $ md ^? docContent . each . bkCollection col . _3
  pure $ toJSON items

getNoteWidget :: Id -> Text -> Handler Widget
getNoteWidget i col = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? kindData . _NoteD
  md <- readNote (note ^. notePath) >>= throwEither (\err -> KMiscError $ "Failed to load node " <> T.pack (note ^. notePath) <> ": " <> err)
  (c, _, items) <- maybe notFound pure $ md ^? docContent . each . bkCollection col
  display <- runInputGet $ displayForm c
  entries <- displayResults display True =<< loadCollection display items
  displayW <- displayResultForm display
  public <- isPublic
  actions <- actionsWidget $ TargetNoteCollection note col
  pure $ do
    unless public $ do
      actions
      Rcs.formsStyle
      [whamlet|
        <form action=@{NoteColR (WId i) col} method=get>
          ^{displayW}
          <input type=submit value="Change display">
      |]
    entries

postNoteColR :: WebId -> Text -> Handler Value
postNoteColR (WId i) col = do
  item <- requireCheckJsonBody
  r <- addToCollection i col item
  unless r $ throwM $ KMiscError "Failed to insert into collection"
  pure $ toJSON ()

getNoteNamedSubR :: WebId -> Text -> Handler Html
getNoteNamedSubR (WId i) sb = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? kindData . _NoteD
  actions <- actionsWidget $ TargetNoteSub note sb
  md <- readNote (note ^. notePath) >>= throwEither (\err -> KMiscError $ "Failed to load node " <> T.pack (note ^. notePath) <> ": " <> err)
  hd <- maybe notFound pure $ md ^? docContent . each . bkNamedSub sb
  (widget, _) <- embedContent False 0 Nothing i [Sub hd] (hd ^. hdChecks)
  defaultLayout $ do
    Rcs.entryStyle
    Rcs.formsStyle
    Rcs.checkboxCode
    PhotoSwipe.photoswipeHeader
    actions
    widget

getNoteNamedCodeR :: WebId -> Text -> Handler Html
getNoteNamedCodeR (WId i) cd = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? kindData . _NoteD
  actions <- actionsWidget $ TargetNoteCode note cd
  md <- readNote (note ^. notePath) >>= throwEither (\err -> KMiscError $ "Failed to load node " <> T.pack (note ^. notePath) <> ": " <> err)
  (attrs, txt) <- maybe notFound pure $ md ^? docContent . each . bkNamedCode cd
  (widget, _) <- embedContent False 0 Nothing i [CodeBlock attrs txt] def
  defaultLayout $ do
    Rcs.entryStyle
    Rcs.formsStyle
    Rcs.checkboxCode
    PhotoSwipe.photoswipeHeader
    actions
    widget
