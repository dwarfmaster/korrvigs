module Korrvigs.Web.Entry.Time (newTemporalRegion) where

import Data.Text (Text)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Web.Backend
import Korrvigs.Web.DB
import qualified Korrvigs.Web.UUID as U
import Yesod

newTemporalRegion :: Text -> Text -> Handler a
newTemporalRegion nm desc = do
  entry <- createEntry TemporalRegion nm desc
  redirect $ EntryR $ U.UUID $ entry_id entry
