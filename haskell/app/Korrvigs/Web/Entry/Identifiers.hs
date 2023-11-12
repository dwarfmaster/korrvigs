module Korrvigs.Web.Entry.Identifiers where

import Data.Text (Text)
import Korrvigs.Classes
import Korrvigs.Classes.Sync (mkMdName)
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Tree (writeNotes)
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.UUID as U
import Yesod hiding (Entity)

-- Create a new namespace and redirect to it
newNamespace :: Text -> Text -> Handler a
newNamespace nm desc = do
  conn <- pgsql
  root <- korrRoot
  let mdName = mkMdName nm
  entry <- liftIO $ newEntry conn root Namespace nm mdName
  writeNotes root entry desc
  redirect $ EntryR $ U.UUID $ entry_id entry
