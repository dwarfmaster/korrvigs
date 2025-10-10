module Korrvigs.Web.Actions
  ( postActHomeR,
    postActEntryR,
    postActSearchR,
    postActNoteColR,
    postActNoteSubR,
    postActNoteCodeR,
    actionsWidget,
    module Korrvigs.Web.Actions.Defs,
  )
where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Catch (catch)
import Data.Default
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Note.AST
import Korrvigs.Query
import Korrvigs.Utils.Base16
import Korrvigs.Web.Actions.Bibtex
import Korrvigs.Web.Actions.Collection
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Actions.EventSync
import Korrvigs.Web.Actions.Export
import Korrvigs.Web.Actions.New
import Korrvigs.Web.Actions.Parent
import Korrvigs.Web.Actions.RSS
import Korrvigs.Web.Actions.Remove
import Korrvigs.Web.Actions.Share
import Korrvigs.Web.Actions.Title
import Korrvigs.Web.Actions.Update
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Search.Form
import Opaleye hiding (not, runUpdate)
import Yesod
import Yesod.Static

data ActionLabel
  = LabRemove
  | LabNewFile
  | LabNewFileDownload
  | LabNewNote
  | LabNewMedia
  | LabNewSyn
  | LabCaptureLink
  | LabShare
  | LabParentAdd
  | LabParentRm
  | LabUpdate
  | LabImportRSS
  | LabNewSyndicate
  | LabRunSyndicate
  | LabEventSync
  | LabExport
  | LabCollection
  | LabBibtex
  | LabUpdateTitle
  | LabRmTitle
  deriving (Eq, Ord, Show, Enum, Bounded)

mkIcon :: Text -> Base16Index -> (Route WebData, Base16Index)
mkIcon icon color = (StaticR $ StaticRoute ["icons", icon <> ".png"] [], color)

actIcon :: ActionLabel -> (Route WebData, Base16Index)
actIcon LabRemove = mkIcon "remove" Base08
actIcon LabNewFile = mkIcon "file" Base0B
actIcon LabNewFileDownload = mkIcon "downloads" Base0B
actIcon LabNewNote = mkIcon "note" Base0B
actIcon LabNewMedia = mkIcon "media" Base0B
actIcon LabNewSyn = mkIcon "rss" Base0B
actIcon LabCaptureLink = mkIcon "link" Base0B
actIcon LabShare = mkIcon "share" Base0E
actIcon LabParentAdd = mkIcon "parent" Base0B
actIcon LabParentRm = mkIcon "parent" Base08
actIcon LabUpdate = mkIcon "upload" Base0F
actIcon LabImportRSS = mkIcon "rss" Base0F
actIcon LabNewSyndicate = mkIcon "rss" Base0E
actIcon LabRunSyndicate = mkIcon "rss" Base0E
actIcon LabEventSync = mkIcon "eventsync" Base0E
actIcon LabExport = mkIcon "export" Base0E
actIcon LabCollection = mkIcon "collection" Base0B
actIcon LabBibtex = mkIcon "bib" Base0E
actIcon LabUpdateTitle = mkIcon "title" Base0E
actIcon LabRmTitle = mkIcon "title" Base08

actName :: ActionLabel -> Text
actName LabRemove = "remove"
actName LabNewFile = "newfile"
actName LabNewFileDownload = "newfiledl"
actName LabNewNote = "newnote"
actName LabNewMedia = "newmedia"
actName LabNewSyn = "newsyn"
actName LabCaptureLink = "capturelink"
actName LabShare = "share"
actName LabParentAdd = "addparent"
actName LabParentRm = "rmparent"
actName LabUpdate = "update"
actName LabImportRSS = "importrss"
actName LabNewSyndicate = "newsyndicate"
actName LabRunSyndicate = "runsyndicate"
actName LabEventSync = "eventsync"
actName LabExport = "export"
actName LabCollection = "collection"
actName LabBibtex = "exportbib"
actName LabUpdateTitle = "updatetitle"
actName LabRmTitle = "rmtitle"

actWidget :: Text -> ActionLabel -> Widget
actWidget formId act = do
  toWidget
    [cassius|
    .action
      width: 1.5em
      height: 1.5em
      border-radius: 0.3em
      cursor: pointer
      float: left
      margin: 0.1em
      padding: 0.25em
    .action-icon
      max-width: 100%
      height: auto
  |]
  [whamlet|
    <div .action style="background-color: var(--#{baseName $ snd $ actIcon act})" data-action-form=#{formId}>
      <img .action-icon src=@{fst $ actIcon act} title=#{actName act}>
  |]

generateForm :: Route WebData -> Text -> AForm Handler a -> Handler Widget
generateForm postUrl title form = do
  let mform = renderDivs form
  (widget, enctype) <- generateFormPost mform
  pure
    [whamlet|
    <form method=post action=@{postUrl} enctype=#{enctype}>
      ^{widget}
      <button>
        #{title}
  |]

genForm :: AForm Handler a -> (ActionTarget -> Text) -> (ActionTarget -> Route WebData) -> ActionTarget -> Handler Widget
genForm form title postUrl tgt = generateForm (postUrl tgt) (title tgt) form

actUrl :: ActionLabel -> ActionTarget -> Route WebData
actUrl lbl (TargetEntry entry) = ActEntryR (actName lbl) (WId $ entry ^. entryName)
actUrl lbl TargetHome = ActHomeR (actName lbl)
actUrl lbl (TargetSearch _ _) = ActSearchR (actName lbl)
actUrl lbl (TargetNoteCollection note col) = ActNoteColR (actName lbl) (WId $ note ^. noteEntry . entryName) col
actUrl lbl (TargetNoteSub note sub) = ActNoteSubR (actName lbl) (WId $ note ^. noteEntry . entryName) sub
actUrl lbl (TargetNoteCode note sub) = ActNoteCodeR (actName lbl) (WId $ note ^. noteEntry . entryName) sub

actForm :: ActionLabel -> ActionTarget -> Handler Widget
actForm l@LabRemove = genForm removeForm removeTitle $ actUrl l
actForm l@LabNewFile = genForm newFileForm newFileTitle $ actUrl l
actForm l@LabNewFileDownload = genForm newFileDlForm newFileDlTitle $ actUrl l
actForm l@LabNewNote = genForm newNoteForm newNoteTitle $ actUrl l
actForm l@LabNewMedia = genForm newMediaForm newMediaTitle $ actUrl l
actForm l@LabNewSyn = genForm newSynForm newSynTitle $ actUrl l
actForm l@LabCaptureLink = genForm captureLinkForm captureLinkTitle $ actUrl l
actForm l@LabShare = genForm shareForm shareTitle $ actUrl l
actForm l@LabParentAdd = genForm parentForm parentAddTitle $ actUrl l
actForm l@LabParentRm = genForm parentForm parentRmTitle $ actUrl l
actForm l@LabUpdate = genForm updateForm updateTitle $ actUrl l
actForm l@LabImportRSS = genForm importRssForm importRssTitle $ actUrl l
actForm l@LabNewSyndicate = genForm syndicateForm syndicateTitle $ actUrl l
actForm l@LabRunSyndicate = genForm runSyndicateForm runSyndicateTitle $ actUrl l
actForm l@LabEventSync = genForm syncEvForm syncEvTitle $ actUrl l
actForm l@LabExport = genForm exportForm exportTitle $ actUrl l
actForm l@LabCollection = genForm colForm colTitle $ actUrl l
actForm l@LabBibtex = genForm bibtexForm bibtexTitle $ actUrl l
actForm l@LabUpdateTitle = genForm titleForm titleTitle $ actUrl l
actForm l@LabRmTitle = genForm rmTitleForm rmTitleTitle $ actUrl l

runPost :: AForm Handler a -> (a -> ActionTarget -> Handler ActionReaction) -> ActionTarget -> Handler ActionReaction
runPost form runner tgt = do
  let mform = renderDivs form
  ((result, _), _) <- runFormPost mform
  case result of
    FormSuccess v ->
      withRunInIO $ \runIO -> liftIO $ catch (runIO $ runner v tgt) $ runIO . catcher
    FormFailure msg -> invalidArgs msg
    FormMissing -> invalidArgs []
  where
    catcher :: KorrvigsError -> Handler ActionReaction
    catcher err = do
      render <- getUrlRenderParams
      pure $
        def
          & reactMsg
            ?~ [hamlet|
        <p>Action failed:

        <pre>#{show err}
      |]
              render

actPost :: ActionLabel -> ActionTarget -> Handler ActionReaction
actPost LabRemove = runPost removeForm runRemove
actPost LabNewFile = runPost newFileForm runNewFile
actPost LabNewFileDownload = runPost newFileDlForm runNewFileDl
actPost LabNewNote = runPost newNoteForm runNewNote
actPost LabNewMedia = runPost newMediaForm runNewMedia
actPost LabNewSyn = runPost newSynForm runNewSyn
actPost LabCaptureLink = runPost captureLinkForm runCaptureLink
actPost LabShare = runPost shareForm runShare
actPost LabParentAdd = runPost parentForm runParentAdd
actPost LabParentRm = runPost parentForm runParentRm
actPost LabUpdate = runPost updateForm runUpdate
actPost LabImportRSS = runPost importRssForm runImportRSS
actPost LabNewSyndicate = runPost syndicateForm runSyndicate
actPost LabRunSyndicate = runPost runSyndicateForm runRunSyndicate
actPost LabEventSync = runPost syncEvForm runSyncEv
actPost LabExport = runPost exportForm runExport
actPost LabCollection = runPost colForm runCol
actPost LabBibtex = runPost bibtexForm runBibtex
actPost LabUpdateTitle = runPost titleForm runTitle
actPost LabRmTitle = runPost rmTitleForm runRmTitle

evaluateCond :: ActionTarget -> ActionCond -> Handler Bool
evaluateCond _ ActCondNever = pure False
evaluateCond _ ActCondAlways = pure True
evaluateCond tgt (ActCondQuery q) = case targetId tgt of
  Nothing -> pure False
  Just sqlI -> do
    r <- rSelectOne $ do
      (e, ()) <- compile q $ const $ pure ()
      where_ $ e ^. sqlEntryId .== sqlInt4 sqlI
      pure ()
    pure $ isJust r
  where
    targetId (TargetEntry e) = Just $ e ^. entryId
    targetId (TargetNoteCollection note _) = Just $ note ^. noteEntry . entryId
    targetId (TargetNoteSub note _) = Just $ note ^. noteEntry . entryId
    targetId (TargetNoteCode note _) = Just $ note ^. noteEntry . entryId
    targetId _ = Nothing
evaluateCond tgt (ActCondAnd cds) =
  getAll . fold <$> mapM (fmap All . evaluateCond tgt) cds
evaluateCond tgt (ActCondOr cds) =
  getAny . fold <$> mapM (fmap Any . evaluateCond tgt) cds
evaluateCond tgt (ActCondNot cd) =
  not <$> evaluateCond tgt cd

runActCond :: (ActionTarget -> ActionCond) -> ActionTarget -> Handler Bool
runActCond f tgt = evaluateCond tgt $ f tgt

actCond :: ActionLabel -> ActionTarget -> Handler Bool
actCond LabRemove = runActCond removeTarget
actCond LabNewFile = runActCond newTarget
actCond LabNewFileDownload = runActCond newTarget
actCond LabNewNote = runActCond newTarget
actCond LabNewMedia = runActCond newTarget
actCond LabNewSyn = runActCond newTarget
actCond LabCaptureLink = runActCond captureLinkTarget
actCond LabShare = runActCond shareTarget
actCond LabParentAdd = runActCond parentTarget
actCond LabParentRm = runActCond parentTarget
actCond LabUpdate = runActCond updateTarget
actCond LabImportRSS = runActCond importRssTarget
actCond LabNewSyndicate = runActCond syndicateTarget
actCond LabRunSyndicate = runActCond runSyndicateTarget
actCond LabEventSync = runActCond syncEvTarget
actCond LabExport = runActCond exportTarget
actCond LabCollection = runActCond colTarget
actCond LabBibtex = runActCond bibtexTarget
actCond LabUpdateTitle = runActCond titleTarget
actCond LabRmTitle = runActCond rmTitleTarget

postHandler :: ActionLabel -> ActionTarget -> Handler Value
postHandler lbl tgt = toJSON <$> actPost lbl tgt

parseActionName :: Text -> Handler ActionLabel
parseActionName nm = maybe notFound pure $ M.lookup nm names
  where
    names = M.fromList $ (actName &&& id) <$> [minBound .. maxBound]

postActEntryR :: Text -> WebId -> Handler Value
postActEntryR nm (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> do
      act <- parseActionName nm
      postHandler act $ TargetEntry entry

postActHomeR :: Text -> Handler Value
postActHomeR nm = do
  act <- parseActionName nm
  postHandler act TargetHome

postActSearchR :: Text -> Handler Value
postActSearchR nm = do
  act <- parseActionName nm
  tz <- liftIO getCurrentTimeZone
  let mktz = fmap $ flip ZonedTime tz
  query <- runInputPost $ queryForm mktz Nothing
  display <- runInputPost $ displayForm ColList
  postHandler act $ TargetSearch query display

postActNoteColR :: Text -> WebId -> Text -> Handler Value
postActNoteColR nm (WId i) col =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. entryKindData of
      NoteD note -> do
        act <- parseActionName nm
        postHandler act $ TargetNoteCollection note col
      _ -> notFound

postActNoteSubR :: Text -> WebId -> Text -> Handler Value
postActNoteSubR nm (WId i) col =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. entryKindData of
      NoteD note -> do
        act <- parseActionName nm
        postHandler act $ TargetNoteSub note col
      _ -> notFound

postActNoteCodeR :: Text -> WebId -> Text -> Handler Value
postActNoteCodeR nm (WId i) col =
  load i >>= \case
    Nothing -> notFound
    Just entry -> case entry ^. entryKindData of
      NoteD note -> do
        act <- parseActionName nm
        postHandler act $ TargetNoteCode note col
      _ -> notFound

actionsWidget :: ActionTarget -> Handler Widget
actionsWidget tgt = do
  templatesId <- newIdent
  containerId <- newIdent
  actions <- listActions
  widgets <- forM actions $ \act -> do
    form <- actForm act tgt
    formId <- newIdent
    pure (formId, form, actWidget formId act)
  pure $ do
    Rcs.actionsCode
    toWidget
      [cassius|
      ##{templatesId}
        display: none
      ##{containerId}
        width: 100%
      #actions-form-container
        width: 100%
    |]
    toWidget $ case tgt of
      TargetSearch _ _ -> [julius|setupActions("query-form");|]
      _ -> [julius|setupActions();|]
    [whamlet|
      <div ##{templatesId}>
        $forall (i,widget,_) <- widgets
          <div ##{i}>
            ^{widget}
      <div ##{containerId}>
        $forall (_,_,widget) <- widgets
          ^{widget}
        <div style="clear:both">
      <div #actions-form-container>
    |]
  where
    listActions = filterM (`actCond` tgt) [minBound .. maxBound]
