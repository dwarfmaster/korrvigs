module Korrvigs.Web.Form (titledForm, nameDescForm) where

import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Web.Backend
import Yesod

toS :: Text -> FieldSettings Korrvigs
toS = fromString . T.unpack

titledForm ::
  Text ->
  Text ->
  (Text -> Text -> a) ->
  Html ->
  MForm Handler (FormResult a, Widget)
titledForm kind desc constructor =
  renderDivs $
    constructor
      <$> areq textField (toS kind) Nothing
      <*> (unTextarea <$> areq textareaField (toS desc) Nothing)

nameDescForm ::
  Text ->
  (Text -> Text -> a) ->
  Html ->
  MForm Handler (FormResult a, Widget)
nameDescForm kind = titledForm kind "Description"
