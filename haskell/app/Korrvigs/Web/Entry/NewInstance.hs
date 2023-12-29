{-# LANGUAGE LambdaCase #-}

module Korrvigs.Web.Entry.NewInstance (newInstance) where

import Control.Arrow ((&&&))
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Text (Text)
import Data.UUID (UUID)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Web.Backend
import Korrvigs.Web.DB (findClass)
import Korrvigs.Web.Entry.Format (newFormat)
import Korrvigs.Web.Entry.Identifiers (newNamespace)
import Korrvigs.Web.Entry.OntologyClass (newClass)
import Korrvigs.Web.Entry.OntologyRelation (newOntologyRelation)
import Korrvigs.Web.Entry.Time (newTemporalRegion)
import Korrvigs.Web.Entry.Types
import qualified Korrvigs.Web.Form as Form
import qualified Korrvigs.Web.Ressources as Rcs
import qualified Korrvigs.Web.UUID as U
import Text.Pandoc.Builder (rawBlock)
import Yesod hiding (Entity)

data NewInstance
  = NewOntologyClass Class Text Text
  | NewNamespace Text Text
  | NewFormat Text Text Text
  | NewTemporalRegion Text Text
  | NewOntologyRelation Text Text Text

clsNewForm :: Class -> Maybe (Html -> MForm Handler (FormResult NewInstance, Widget))
clsNewForm Entity = Nothing
clsNewForm OntologyClass =
  Just $
    renderDivs $
      NewOntologyClass
        <$> areq sel "Parent class" Nothing
        <*> areq textField "Class" Nothing
        <*> (unTextarea <$> areq textareaField "Description" Nothing)
  where
    sel = selectField $ optionsPairs $ sortBy (\(nm1, _) (nm2, _) -> compare nm1 nm2) $ (name &&& id) <$> [minBound .. maxBound]
clsNewForm Namespace = Just $ Form.nameDescForm "Namespace" NewNamespace
clsNewForm DataFormatSpecification =
  Just $
    renderDivs $
      NewFormat
        <$> areq textField "Format" Nothing
        <*> areq textField "MIME type" Nothing
        <*> (unTextarea <$> areq textareaField "Description" Nothing)
clsNewForm TemporalRegion = Just $ Form.nameDescForm "Abstract temporal region" NewTemporalRegion
clsNewForm OntologyRelation =
  Just $
    renderDivs $
      NewOntologyRelation
        <$> areq textField "Relation" Nothing
        <*> (unTextarea <$> areq textareaField "Description" Nothing)
        <*> (unTextarea <$> areq textareaField "Structure" Nothing)
clsNewForm cls = clsNewForm (isA cls)

-- Create the new instance and redirect to it
doNewInstance :: NewInstance -> Handler a
doNewInstance (NewOntologyClass cls nm desc) = newClass (name cls) nm desc
doNewInstance (NewFormat nm mime desc) = newFormat nm mime desc
doNewInstance (NewNamespace nm desc) = newNamespace nm desc
doNewInstance (NewTemporalRegion nm desc) = newTemporalRegion nm desc
doNewInstance (NewOntologyRelation nm desc dhall) = newOntologyRelation nm desc dhall

newInstanceForm :: Class -> Maybe (Html -> MForm Handler (FormResult NewInstance, Widget))
newInstanceForm = clsNewForm

newInstanceWidget :: UUID -> Class -> Handler (Maybe Widget)
newInstanceWidget uuid cls = case newInstanceForm cls of
  Nothing -> pure Nothing
  Just form -> do
    (widget, enctype) <- generateFormPost form
    pure $
      Just $
        Rcs.formStyle
          >> [whamlet|
           <form #class-new-sub method="post" action=@{EntryR (U.UUID uuid)} enctype=#{enctype}>
             ^{widget}
             <button type="submit">Create
         |]

newInstanceProcess :: Class -> Handler TypedContent
newInstanceProcess cls = case newInstanceForm cls of
  Nothing -> notFound
  Just form -> do
    ((result, _), _) <- runFormPost form
    case result of
      FormSuccess ninstance -> doNewInstance ninstance
      FormFailure err ->
        pure $ toTypedContent $ toJSON err
      FormMissing -> pure $ toTypedContent ()

newInstance :: Entry -> Handler WidgetMap
newInstance entry =
  findClass (entry_id entry) >>= \case
    Nothing -> pure M.empty
    Just cls -> do
      mform <- newInstanceWidget (entry_id entry) cls
      pure $ case mform of
        Nothing -> M.empty
        Just form ->
          M.fromList
            [ ("New Instance", Left $ rawBlock "widget" "New Instance Widget"),
              ("New Instance Widget", Right (form, newInstanceProcess cls))
            ]
