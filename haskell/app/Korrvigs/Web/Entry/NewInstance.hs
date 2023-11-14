{-# LANGUAGE LambdaCase #-}

module Korrvigs.Web.Entry.NewInstance (newInstance) where

import Control.Arrow ((&&&))
import Data.List (sortBy)
import Data.Map (Map)
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
import Korrvigs.Web.Entry.Time (newTemporalRegion)
import qualified Korrvigs.Web.Form as Form
import Korrvigs.Web.Method
import qualified Korrvigs.Web.Ressources as Rcs
import qualified Korrvigs.Web.UUID as U
import Yesod hiding (Entity)

data NewInstance
  = NewOntologyClass Class Text Text
  | NewNamespace Text Text
  | NewFormat Text Text Text
  | NewTemporalRegion Text Text

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
clsNewForm cls = clsNewForm (isA cls)

-- Create the new instance and redirect to it
doNewInstance :: NewInstance -> Handler a
doNewInstance (NewOntologyClass cls nm desc) = newClass (name cls) nm desc
doNewInstance (NewFormat nm mime desc) = newFormat nm mime desc
doNewInstance (NewNamespace nm desc) = newNamespace nm desc
doNewInstance (NewTemporalRegion nm desc) = newTemporalRegion nm desc

newInstanceForm :: Class -> Maybe (Html -> MForm Handler (FormResult NewInstance, Widget))
newInstanceForm cls = identifyForm "NewInstance" <$> clsNewForm cls

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

newInstanceProcess :: Class -> Handler (Maybe Widget)
newInstanceProcess cls = case newInstanceForm cls of
  Nothing -> pure Nothing
  Just form -> do
    ((result, _), _) <- runFormPost form
    case result of
      FormSuccess ninstance -> doNewInstance ninstance
      FormFailure err ->
        pure $
          Just $
            [whamlet|
            <p>Invalid input:
              <ul>
                $forall msg <- err
                  <li> #{msg}
          |]
      FormMissing -> pure $ Nothing

newInstance :: Method -> Entry -> Handler (Map String Widget)
newInstance method entry =
  findClass (entry_id entry) >>= \case
    Nothing -> pure M.empty
    Just cls -> do
      newIns <-
        selectOpt (method == methodGet) "New Instance" $
          newInstanceWidget (entry_id entry) cls
      prcIns <-
        selectOpt (method == methodPost) "New Instance" $
          newInstanceProcess cls
      pure $ M.fromList $ newIns ++ prcIns
