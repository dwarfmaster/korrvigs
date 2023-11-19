module Korrvigs.Web.Entry.OntologyRelation (newOntologyRelation) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text.IO (writeFile)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Korrvigs.Classes
import Korrvigs.Classes.Sync (mkMdName)
import Korrvigs.Definition
import Korrvigs.Entry
import qualified Korrvigs.Tree as Tree
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.UUID as U
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Yesod
import Prelude hiding (writeFile)

newOntologyRelation :: Text -> Text -> Text -> Handler a
newOntologyRelation nm desc dhall = do
  conn <- pgsql
  root <- korrRoot
  entry <- liftIO $ withSystemTempDirectory "korrvigs" $ \temp -> do
    let path = temp </> "relation.dhall"
    writeFile path dhall
    withTransaction conn $ do
      entry <- newEntry conn root OntologyRelation nm $ mkMdName nm
      void $ newEntity conn root entry File (Just path) Nothing
      return entry
  Tree.writeNotes root entry desc
  redirect $ EntryR $ U.UUID $ entry_id entry
