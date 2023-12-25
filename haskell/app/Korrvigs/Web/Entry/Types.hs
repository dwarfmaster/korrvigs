module Korrvigs.Web.Entry.Types where

import Data.Map
import Data.Text (Text)
import Korrvigs.Web.Backend
import Text.Pandoc.Builder
import Yesod

type InteractiveWidget = (Widget, Handler TypedContent)

type WidgetMap = Map String (Either Blocks InteractiveWidget)

trivialHandler :: Handler TypedContent
trivialHandler = pure $ toTypedContent ("" :: Text)
