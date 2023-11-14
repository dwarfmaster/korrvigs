module Korrvigs.Web.Entry.Identifiers where

import Data.Text (Text)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Web.Backend
import Korrvigs.Web.DB
import qualified Korrvigs.Web.UUID as U
import Yesod hiding (Entity)

-- Create a new namespace and redirect to it
newNamespace :: Text -> Text -> Handler a
newNamespace nm desc = do
  entry <- createEntry Namespace nm desc
  redirect $ EntryR $ U.UUID $ entry_id entry
