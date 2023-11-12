module Korrvigs.Web.Entry.Identifiers where

import Data.Text (Text)
import Data.Text.IO (writeFile)
import Korrvigs.Classes
import Korrvigs.Classes.Sync (mkMdName)
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.UUID as U
import Yesod hiding (Entity)
import Prelude hiding (writeFile)

-- Create a new namespace and redirect to it
newNamespace :: Text -> Text -> Handler a
newNamespace nm desc = do
  conn <- pgsql
  root <- korrRoot
  let mdName = mkMdName nm
  entry <- liftIO $ newEntry conn root Namespace nm mdName
  let mdPath = entryMdPath root entry
  liftIO $ writeFile mdPath desc
  redirect $ EntryR $ U.UUID $ entry_id entry
