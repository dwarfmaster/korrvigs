module Korrvigs.Web.Actions
  ( postActColR,
    postActHomeR,
    postActEntryR,
    actionsWidget,
    module Korrvigs.Web.Actions.Defs,
  )
where

import Control.Arrow
import Control.Monad
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Utils.Base16
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Actions.New
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Yesod
import Yesod.Static

data ActionLabel
  = LabNewFile
  | LabNewNote
  | LabNewLink
  | LabNewMedia
  deriving (Eq, Ord, Show, Enum, Bounded)

actIcon :: ActionLabel -> (Route WebData, Base16Index)
actIcon LabNewFile = (StaticR $ StaticRoute ["icons", "file.png"] [], Base0B)
actIcon LabNewNote = (StaticR $ StaticRoute ["icons", "note.png"] [], Base0B)
actIcon LabNewLink = (StaticR $ StaticRoute ["icons", "link.png"] [], Base0B)
actIcon LabNewMedia = (StaticR $ StaticRoute ["icons", "media.png"] [], Base0B)

actName :: ActionLabel -> Text
actName LabNewFile = "newfile"
actName LabNewNote = "newnote"
actName LabNewLink = "newlink"
actName LabNewMedia = "newmedia"

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
      <img .action-icon src=@{fst $ actIcon act} alt=#{actName act}>
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
actUrl lbl (TargetEntry i) = ActEntryR (actName lbl) (WId i)
actUrl lbl TargetHome = ActHomeR (actName lbl)
actUrl lbl (TargetCollection col) = ActColR (actName lbl) col

actForm :: ActionLabel -> ActionTarget -> Handler Widget
actForm l@LabNewFile = genForm newFileForm newFileTitle $ actUrl l
actForm l@LabNewNote = genForm newNoteForm newNoteTitle $ actUrl l
actForm l@LabNewLink = genForm newLinkForm newLinkTitle $ actUrl l
actForm l@LabNewMedia = genForm newMediaForm newMediaTitle $ actUrl l

runPost :: AForm Handler a -> (a -> ActionTarget -> Handler ActionReaction) -> ActionTarget -> Handler ActionReaction
runPost form runner tgt = do
  let mform = renderDivs form
  ((result, _), _) <- runFormPost mform
  case result of
    FormSuccess v -> runner v tgt
    FormFailure msg -> invalidArgs msg
    FormMissing -> invalidArgs []

actPost :: ActionLabel -> ActionTarget -> Handler ActionReaction
actPost LabNewFile = runPost newFileForm runNewFile
actPost LabNewNote = runPost newNoteForm runNewNote
actPost LabNewLink = runPost newLinkForm runNewLink
actPost LabNewMedia = runPost newMediaForm runNewMedia

actCond :: ActionLabel -> ActionTarget -> Bool
actCond LabNewFile = newTarget
actCond LabNewNote = newTarget
actCond LabNewLink = newTarget
actCond LabNewMedia = newTarget

postHandler :: ActionLabel -> ActionTarget -> Handler Value
postHandler lbl tgt = toJSON <$> actPost lbl tgt

parseActionName :: Text -> Handler ActionLabel
parseActionName nm = maybe notFound pure $ M.lookup nm names
  where
    names = M.fromList $ (actName &&& id) <$> [minBound .. maxBound]

postActEntryR :: Text -> WebId -> Handler Value
postActEntryR nm (WId i) = do
  act <- parseActionName nm
  postHandler act $ TargetEntry i

postActHomeR :: Text -> Handler Value
postActHomeR nm = do
  act <- parseActionName nm
  postHandler act TargetHome

postActColR :: Text -> [Text] -> Handler Value
postActColR nm col = do
  act <- parseActionName nm
  postHandler act $ TargetCollection col

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
    toWidget [julius|setupActions();|]
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
