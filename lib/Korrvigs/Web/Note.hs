module Korrvigs.Web.Note
  ( getNoteFuzzyR,
    getNoteR,
    postNoteR,
    getNoteSubR,
    postNoteSubR,
    postNoteSubActR,
    getNoteColR,
    postNoteColR,
    getNoteCodeR,
    getNoteNamedSubR,
    getNoteNamedCodeR,
    getNoteColEditR,
    postNoteColEditR,
    getNoteSyndicateR,
    getNoteSyndicateSingleR,
  )
where

import Conduit hiding (fuse)
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit.Combinators (fold)
import Data.Default
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Time.Format.ISO8601
import Korrvigs.Compute.SQL
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Monad.Sync (syncFileOfKind)
import Korrvigs.Monad.Syndicate
import Korrvigs.Note
import Korrvigs.Note.AST
import Korrvigs.Note.Edit
import Korrvigs.Note.Languages
import Korrvigs.Note.Pandoc
import qualified Korrvigs.Syndicate.Run as Syn
import Korrvigs.Web.Actions
import Korrvigs.Web.Backend
import Korrvigs.Web.Entry.Note (embedContent, resultWidget)
import qualified Korrvigs.Web.JS.Fuse as Fuse
import qualified Korrvigs.Web.JS.PhotoSwipe as PhotoSwipe
import Korrvigs.Web.Note.Col
import Korrvigs.Web.Note.Syndicate
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Search
import Korrvigs.Web.Search.Form
import Korrvigs.Web.Search.Results
import qualified Network.URI as URI
import Opaleye
import System.IO
import Yesod hiding (Header, check)

getNoteR :: WebId -> Handler LT.Text
getNoteR (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. entryKindData of
      NoteD note -> liftIO $ LEnc.decodeUtf8 <$> BSL.readFile (note ^. notePath)
      _ -> notFound

postNoteR :: WebId -> Handler LT.Text
postNoteR (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. entryKindData of
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
            syncFileOfKind i path (entry ^. entryId) Note
            redirect $ NoteR $ WId i
      _ -> notFound

getNoteSubR :: WebId -> WebAnyLoc -> Handler TypedContent
getNoteSubR (WId i) (WLoc loc) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. entryKindData of
      NoteD note ->
        readNote (note ^. notePath) >>= \case
          Left err -> throwM $ KMiscError err
          Right doc -> case loc of
            LocCode lc -> case doc ^? code lc of
              Nothing -> notFound
              Just txt -> pure $ toTypedContent txt
            LocSub lc -> case doc ^? sub lc of
              Nothing -> notFound
              Just hd -> pure $ toTypedContent $ LEnc.decodeUtf8 $ writeHeaderLazy hd (doc ^. docComputations)
            LocCheck lc -> case doc ^? check lc of
              Nothing -> notFound
              Just cb -> pure $ toTypedContent $ renderTaskStatus cb
            LocTask lc -> case doc ^? task lc . tskStatus of
              Nothing -> notFound
              Just tk -> pure $ toTypedContent $ renderTaskStatus tk
            LocSyn lc -> case doc ^? _syn lc . _4 of
              Nothing -> notFound
              Just syns -> do
                ids <- resolveSyndicate syns
                render <- getUrlRender
                let content = fmap ((unId . view _1) &&& (render . EntryR . WId . view _1)) ids
                pure $ toTypedContent $ toJSON content
      _ -> notFound

postNoteSubR :: WebId -> WebAnyLoc -> Handler LT.Text
postNoteSubR (WId i) (WLoc loc) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. entryKindData of
      NoteD note ->
        readNote (note ^. notePath) >>= \case
          Left err -> throwM $ KMiscError err
          Right doc -> do
            body <- runConduit $ rawRequestBody .| fold
            let txt = Enc.decodeUtf8 body
            mdoc <- case loc of
              LocCode lc -> pure $ Just $ setCode lc doc txt
              LocSub lc -> do
                let lvl = length $ lc ^. subOffsets
                hd <- readNoteFromText (parseTopBlocks lvl) txt
                case hd of
                  Left err -> throwM $ KMiscError err
                  Right bks -> do
                    let shifted = bks & each . _Sub %~ shiftSubTo lvl
                    pure $ Just $ doc & subs lc .~ shifted
              LocCheck lc -> do
                cb <- parseTaskStatus txt
                pure $ Just $ setCheck lc doc cb
              LocTask lc -> do
                tk <- parseTaskStatus txt
                pure $
                  Just $
                    doc
                      & task lc . tskStatus .~ tk
                      & task lc . tskStatusName .~ txt
              LocSyn lc -> case doc ^? _syn lc . _4 of
                Nothing -> notFound
                Just syns -> do
                  ids <- resolveSyndicate syns
                  forM_ ids $ \(_, _, syn) -> Syn.run syn
                  pure Nothing
            forM_ mdoc $ \ndoc -> do
              let path = note ^. notePath
              fd <- liftIO $ openFile path WriteMode
              writeNote fd ndoc >>= \case
                Just err -> do
                  liftIO $ hClose fd
                  throwM $ KMiscError err
                Nothing -> pure ()
              liftIO $ hClose fd
              syncFileOfKind i path (entry ^. entryId) Note
            redirect $ NoteSubR (WId i) (WLoc loc)
      _ -> notFound
  where
    parseTaskStatus txt = maybe (throwM $ KMiscError $ "\"" <> txt <> "\" is not a valid task state") pure $ parseStatusName txt
    shiftSubTo :: Int -> Header -> Header
    shiftSubTo lvl hd | hd ^. hdLevel == lvl = hd
    shiftSubTo lvl hd = transform (hdLevel %~ (+ (lvl - hd ^. hdLevel))) hd

postNoteSubActR :: WebId -> WebAnyLoc -> Handler Text
postNoteSubActR (WId i) (WLoc (LocSub loc)) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. entryKindData of
      NoteD note ->
        readNote (note ^. notePath) >>= \case
          Left err -> throwM $ KMiscError err
          Right doc -> do
            body <- runConduit $ rawRequestBody .| fold
            let dat = T.strip <$> T.lines (Enc.decodeUtf8 body)
            (redirUrl, redirEmbed, act) <- case dat of
              [redirUrl, redirEmbed, act] -> case parseDeepEmbedLoc redirEmbed of
                Right l -> pure (redirUrl, l, act)
                Left err -> invalidArgs [err]
              _ -> invalidArgs []
            ndoc <- case act of
              "sub-first" -> pure $ addSubHeaderFirst loc doc
              "sub-last" -> pure $ addSubHeaderLast loc doc
              "header-after" -> pure $ addHeaderAfter loc doc
              "header-before" -> pure $ addHeaderBefore loc doc
              "finish-task" -> pure $ finishTask loc doc
              _ -> notFound
            let path = note ^. notePath
            fd <- liftIO $ openFile path WriteMode
            writeNote fd ndoc >>= \case
              Just err -> do
                liftIO $ hClose fd
                throwM $ KMiscError err
              Nothing -> pure ()
            liftIO $ hClose fd
            syncFileOfKind i path (entry ^. entryId) Note
            let nloc = case act of
                  "sub-first" -> loc & subOffsets %~ (0 :)
                  "sub-last" ->
                    let nsubs = length $ doc ^.. sub loc . hdContent . each . _Sub
                     in loc & subOffsets %~ (nsubs :)
                  "header-after" -> loc & subOffsets . _head %~ (+ 1)
                  _ -> loc
            let locParam = URI.escapeURIString URI.isUnescapedInURIComponent $ T.unpack $ renderEmbeddedLoc (redirEmbed, nloc)
            let param = case act of
                  "finish-task" -> "open"
                  _ -> "edit"
            pure $ redirUrl <> "?" <> param <> "=" <> T.pack locParam
      _ -> notFound
postNoteSubActR _ _ = notFound

getNoteColR :: WebId -> Text -> Handler TypedContent
getNoteColR (WId i) col = do
  accept <- lookupHeader "accept"
  if maybe False ("application/json" `BS.isPrefixOf`) accept
    then toTypedContent <$> getNoteJson i col
    else toTypedContent <$> (getNoteWidget i col >>= defaultLayout)

getNoteJson :: Id -> Text -> Handler Value
getNoteJson i col = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? entryKindData . _NoteD
  md <- readNote (note ^. notePath) >>= throwEither (\err -> KMiscError $ "Failed to load node " <> T.pack (note ^. notePath) <> ": " <> err)
  items <- maybe notFound pure $ md ^? docContent . each . bkCollection col . _3
  pure $ toJSON items

getNoteWidget :: Id -> Text -> Handler Widget
getNoteWidget i col = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? entryKindData . _NoteD
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
      Rcs.formsStyle CssR
      [whamlet|
        <form action=@{NoteColR (WId i) col} method=get>
          ^{displayW}
          <input type=submit value="Change display">
      |]
      [whamlet|
        <ul>
          <li>
            <a href=@{EntryR $ WId i}>
              Back to entry
          <li>
            <a href=@{NoteColEditR (WId i) col}>
              Edit
      |]
    entries

postNoteColR :: WebId -> Text -> Handler Value
postNoteColR (WId i) col = do
  item <- requireCheckJsonBody
  r <- addToCollection i col item
  unless r $ throwM $ KMiscError "Failed to insert into collection"
  pure $ toJSON ()

getNoteCodeR :: WebId -> WebAnyLoc -> Handler Text
getNoteCodeR (WId i) (WLoc (LocCode cd)) = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? entryKindData . _NoteD
  md <- readNote (note ^. notePath) >>= throwEither (\err -> KMiscError $ "Failed to load node " <> T.pack (note ^. notePath) <> ": " <> err)
  case md ^? codeFull cd of
    Nothing -> notFound
    Just (attr, txt) -> do
      let codeName = if T.null (attr ^. attrId) then renderCodeLoc cd else attr ^. attrId
      let ext = findExtension $ attr ^. attrClasses
      let filename = T.replace ":" "_" $ unId i <> "_" <> codeName <> "." <> ext
      addHeader "Content-Disposition" $ "inline, filename=\"" <> filename <> "\""
      pure txt
  where
    findExtension :: [Text] -> Text
    findExtension classes =
      fromMaybe "txt" $ getAlt $ mconcat $ Alt . lookupExt <$> classes
getNoteCodeR _ _ = notFound

lookupExt :: Text -> Maybe Text
lookupExt l = languagesMap ^? at l . _Just . langExt

getNoteNamedSubR :: WebId -> Text -> Handler Html
getNoteNamedSubR (WId i) sb = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? entryKindData . _NoteD
  actions <- actionsWidget $ TargetNoteSub note sb
  md <- readNote (note ^. notePath) >>= throwEither (\err -> KMiscError $ "Failed to load node " <> T.pack (note ^. notePath) <> ": " <> err)
  hd <- maybe notFound pure $ md ^? docContent . each . bkNamedSub sb
  (widget, _) <- embedContent False False 0 Nothing (i, DeepEmbedLoc []) i md [Sub hd] (hd ^. hdChecks)
  public <- isPublic
  defaultLayout $ do
    Rcs.entryStyle CssR
    Rcs.formsStyle CssR
    Rcs.checkboxCode StaticR
    PhotoSwipe.photoswipeHeader
    unless public actions
    widget

getNoteNamedCodeR :: WebId -> Text -> Handler Html
getNoteNamedCodeR (WId i) cd = do
  entry <- load i >>= maybe notFound pure
  note <- maybe notFound pure $ entry ^? entryKindData . _NoteD
  actions <- actionsWidget $ TargetNoteCode note cd
  md <- readNote (note ^. notePath) >>= throwEither (\err -> KMiscError $ "Failed to load node " <> T.pack (note ^. notePath) <> ": " <> err)
  (attrs, txt) <- maybe notFound pure $ md ^? docContent . each . bkNamedCode cd
  (widget, _) <- embedContent False False 0 Nothing (i, DeepEmbedLoc []) i md [CodeBlock attrs txt] def
  let result = M.lookup cd $ md ^. docComputations
  rwidget <- case result of
    Nothing -> pure mempty
    Just res -> resultWidget i cd (res ^. cmpResType) (res ^. cmpResData)
  public <- isPublic
  defaultLayout $ do
    Rcs.entryStyle CssR
    Rcs.formsStyle CssR
    Rcs.checkboxCode StaticR
    PhotoSwipe.photoswipeHeader
    unless public actions
    forM_ result $ \r ->
      [whamlet|
        <p>Run in #{show (view cmpResRuntime r)}ms (#{iso8601Show (view cmpResDate r)})
      |]
    widget
    [whamlet|
      <div .computation-result>
        ^{rwidget}
    |]

getNoteFuzzyR :: Handler Html
getNoteFuzzyR = do
  notes :: [EntryRowR] <- rSelect $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryKind .== sqlKind Note
    private <- selectTextMtdt Private $ entry ^. sqlEntryId
    where_ $ matchNullable (sqlBool True) (./= sqlStrictText "yes") private
    pure entry
  items <- forM notes $ Fuse.itemFromEntry . (view sqlEntryName &&& view sqlEntryTitle)
  fuse <- Fuse.widget items
  defaultLayout $ do
    Fuse.header
    fuse
