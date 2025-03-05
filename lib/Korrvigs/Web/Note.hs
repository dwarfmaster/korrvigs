module Korrvigs.Web.Note
  ( getNoteR,
    postNoteR,
    getNoteSubR,
    postNoteSubR,
  )
where

import Conduit
import Control.Lens
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit.Combinators (fold)
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Actions.Load
import Korrvigs.Actions.Sync (processRelData)
import Korrvigs.Entry
import Korrvigs.Monad (KorrvigsError (KMiscError))
import Korrvigs.Note
import Korrvigs.Note.Pandoc
import Korrvigs.Note.Sync (dSyncOneImpl)
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
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
            dSyncOneImpl path >>= processRelData i
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
    renderTaskStatus TaskOngoing = "ongoing"
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
                hd <- readNoteFromText (parseHeader lvl) txt
                case hd of
                  Left err -> throwM $ KMiscError err
                  Right Nothing -> throwM $ KMiscError "Partial markdown file is not a single header"
                  Right (Just hdv) -> pure $ setSub lc doc hdv
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
            dSyncOneImpl path >>= processRelData i
            redirect $ NoteSubR (WId i) (WLoc loc)
      _ -> notFound
  where
    parseTaskStatus "todo" = pure TaskTodo
    parseTaskStatus "started" = pure TaskOngoing
    parseTaskStatus "blocked" = pure TaskBlocked
    parseTaskStatus "done" = pure TaskDone
    parseTaskStatus "dont" = pure TaskDont
    parseTaskStatus txt = throwM $ KMiscError $ "\"" <> txt <> "\" is not a valid task state"
