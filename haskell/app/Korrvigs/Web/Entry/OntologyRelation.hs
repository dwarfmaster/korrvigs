module Korrvigs.Web.Entry.OntologyRelation (newOntologyRelation, schemaWidget) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text.IO (writeFile)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Korrvigs.Classes
import Korrvigs.Classes.Colors (classBase)
import Korrvigs.Classes.Sync (mkMdName)
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Relations (Type (..), parseDhall)
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

schemaWidget :: Entry -> Handler Widget
schemaWidget entry = do
  root <- korrRoot
  let path = subPath root (entry_id entry) "relation.dhall"
  res <- liftIO $ parseDhall path
  case res of
    Left err -> pure [whamlet|<p> Error: #{err}|]
    Right types -> do
      rendered <- mapM (\(nm, tp) -> (nm,) <$> renderTp tp) types
      pure
        [whamlet|
        <ul>
          $forall (nm,tp) <- rendered
            <li> #{nm}: ^{tp}
      |]
  where
    render :: Text -> Widget
    render str =
      [whamlet|
            <span .entry-class style="background-color: var(--base05)">
              #{str}
          |]
    renderTp :: Type -> Handler Widget
    renderTp TText = pure $ render "Text"
    renderTp TUUID = pure $ render "UUID"
    renderTp (TClass cls) = do
      conn <- pgsql
      clsEntry <- lookupEntryByName' conn OntologyClass $ name cls
      pure
        [whamlet|
            <span .entry-class style="background-color: var(--base#{classBase cls})">
              <a href=@{EntryR (U.UUID (entry_id clsEntry))}>
                #{name cls}
          |]
