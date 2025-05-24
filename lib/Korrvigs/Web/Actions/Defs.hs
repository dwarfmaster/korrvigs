module Korrvigs.Web.Actions.Defs where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Default
import Data.List
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Query
import Text.Blaze.Renderer.Text
import Yesod

data ActionTarget
  = TargetEntry Entry
  | TargetHome
  | TargetCollection [Text]
  | TargetSearch Query

data ActionReaction = ActionReaction
  { _reactMsg :: Maybe Html,
    _reactClipboard :: Maybe Text,
    _reactAlert :: Maybe Text,
    _reactRedirect :: Maybe Text
  }

makeLenses ''ActionReaction

instance Default ActionReaction where
  def = ActionReaction Nothing Nothing Nothing Nothing

instance ToJSON ActionReaction where
  toJSON react =
    object $
      maybe [] (singleton . ("message" .=) . renderMarkup) (react ^. reactMsg)
        ++ maybe [] (singleton . ("clipboard" .=)) (react ^. reactClipboard)
        ++ maybe [] (singleton . ("alert" .=)) (react ^. reactAlert)
        ++ maybe [] (singleton . ("redirect" .=)) (react ^. reactRedirect)
