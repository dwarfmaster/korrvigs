module Korrvigs.Web.Actions
  ( postActColR,
    postActHomeR,
    postActEntryR,
    postActSearchR,
    actionsWidget,
    module Korrvigs.Web.Actions.Defs,
  )
where

import Control.Arrow
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Actions.SQL
import Korrvigs.Entry
import Korrvigs.Utils.Base16
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Actions.EventSync
import Korrvigs.Web.Actions.Export
import Korrvigs.Web.Actions.New
import Korrvigs.Web.Actions.Parent
import Korrvigs.Web.Actions.Remove
import Korrvigs.Web.Actions.Share
import Korrvigs.Web.Actions.Update
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Search.Form
import Yesod
import Yesod.Static

data ActionLabel
  = LabRemove
  | LabNewFile
  | LabNewNote
  | LabNewLink
  | LabNewMedia
  | LabShare
  | LabParentAdd
  | LabParentRm
  | LabUpdate
  | LabEventSync
  | LabExport
  deriving (Eq, Ord, Show, Enum, Bounded)

mkIcon :: Text -> Base16Index -> (Route WebData, Base16Index)
mkIcon icon color = (StaticR $ StaticRoute ["icons", icon <> ".png"] [], color)

actIcon :: ActionLabel -> (Route WebData, Base16Index)
actIcon LabRemove = mkIcon "remove" Base08
actIcon LabNewFile = mkIcon "file" Base0B
actIcon LabNewNote = mkIcon "note" Base0B
actIcon LabNewLink = mkIcon "link" Base0B
actIcon LabNewMedia = mkIcon "media" Base0B
actIcon LabShare = mkIcon "share" Base0E
actIcon LabParentAdd = mkIcon "parent" Base0B
actIcon LabParentRm = mkIcon "parent" Base08
actIcon LabUpdate = mkIcon "upload" Base0E
actIcon LabEventSync = mkIcon "eventsync" Base0E
actIcon LabExport = mkIcon "export" Base0E

actName :: ActionLabel -> Text
actName LabRemove = "remove"
actName LabNewFile = "newfile"
actName LabNewNote = "newnote"
actName LabNewLink = "newlink"
actName LabNewMedia = "newmedia"
actName LabShare = "share"
actName LabParentAdd = "addparent"
actName LabParentRm = "rmparent"
actName LabUpdate = "update"
actName LabEventSync = "eventsync"
actName LabExport = "export"

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
actUrl lbl (TargetEntry entry) = ActEntryR (actName lbl) (WId $ entry ^. name)
actUrl lbl TargetHome = ActHomeR (actName lbl)
actUrl lbl (TargetCollection col) = ActColR (actName lbl) col
actUrl lbl (TargetSearch _ _) = ActSearchR (actName lbl)

actForm :: ActionLabel -> ActionTarget -> Handler Widget
actForm l@LabRemove = genForm removeForm removeTitle $ actUrl l
actForm l@LabNewFile = genForm newFileForm newFileTitle $ actUrl l
actForm l@LabNewNote = genForm newNoteForm newNoteTitle $ actUrl l
actForm l@LabNewLink = genForm newLinkForm newLinkTitle $ actUrl l
actForm l@LabNewMedia = genForm newMediaForm newMediaTitle $ actUrl l
actForm l@LabShare = genForm shareForm shareTitle $ actUrl l
actForm l@LabParentAdd = genForm parentForm parentAddTitle $ actUrl l
actForm l@LabParentRm = genForm parentForm parentRmTitle $ actUrl l
actForm l@LabUpdate = genForm updateForm updateTitle $ actUrl l
actForm l@LabEventSync = genForm syncEvForm syncEvTitle $ actUrl l
actForm l@LabExport = genForm exportForm exportTitle $ actUrl l

runPost :: AForm Handler a -> (a -> ActionTarget -> Handler ActionReaction) -> ActionTarget -> Handler ActionReaction
runPost form runner tgt = do
  let mform = renderDivs form
  ((result, _), _) <- runFormPost mform
  case result of
    FormSuccess v -> runner v tgt
    FormFailure msg -> invalidArgs msg
    FormMissing -> invalidArgs []

actPost :: ActionLabel -> ActionTarget -> Handler ActionReaction
actPost LabRemove = runPost removeForm runRemove
actPost LabNewFile = runPost newFileForm runNewFile
actPost LabNewNote = runPost newNoteForm runNewNote
actPost LabNewLink = runPost newLinkForm runNewLink
actPost LabNewMedia = runPost newMediaForm runNewMedia
actPost LabShare = runPost shareForm runShare
actPost LabParentAdd = runPost parentForm runParentAdd
actPost LabParentRm = runPost parentForm runParentRm
actPost LabUpdate = runPost updateForm runUpdate
actPost LabEventSync = runPost syncEvForm runSyncEv
actPost LabExport = runPost exportForm runExport

actCond :: ActionLabel -> ActionTarget -> Bool
actCond LabRemove = removeTarget
actCond LabNewFile = newTarget
actCond LabNewNote = newTarget
actCond LabNewLink = newTarget
actCond LabNewMedia = newTarget
actCond LabShare = shareTarget
actCond LabParentAdd = parentTarget
actCond LabParentRm = parentTarget
actCond LabUpdate = updateTarget
actCond LabEventSync = syncEvTarget
actCond LabExport = exportTarget

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

postActColR :: Text -> [Text] -> Handler Value
postActColR nm col = do
  act <- parseActionName nm
  postHandler act $ TargetCollection col

postActSearchR :: Text -> Handler Value
postActSearchR nm = do
  act <- parseActionName nm
  tz <- liftIO getCurrentTimeZone
  let mktz = fmap $ flip ZonedTime tz
  query <- runInputPost $ queryForm mktz Nothing
  display <- runInputPost displayForm
  postHandler act $ TargetSearch query display

actionsWidget :: ActionTarget -> Handler Widget
actionsWidget tgt = do
  templatesId <- newIdent
  containerId <- newIdent
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
    actions = filter (`actCond` tgt) [minBound .. maxBound]
