module Korrvigs.Web.Entry.Format (newFormat, inNamespaceSql, inNamespaceAllSql) where

import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Korrvigs.Classes
import Korrvigs.Classes.Sync (mkMdName)
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Relations
import qualified Korrvigs.Tree as Tree
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.UUID as U
import Opaleye ((.==))
import qualified Opaleye as O
import Yesod hiding (Entity)

newFormat :: Text -> Text -> Text -> Handler a
newFormat nm mime desc = do
  conn <- pgsql
  root <- korrRoot
  allEntity <- entry_root <$> lookupEntryByName' conn TemporalRegion "All"
  entry <- liftIO $ withTransaction conn $ do
    entry <- newEntry conn root DataFormatSpecification nm $ mkMdName nm
    identifier <- getMimeFor conn root entry allEntity mime
    void $
      O.runInsert conn $
        O.Insert
          { O.iTable = denotesAtRel,
            O.iRows =
              [ ( O.sqlInt8 identifier,
                  O.sqlInt8 $ entity_id $ entry_root entry,
                  O.sqlInt8 (entity_id allEntity)
                )
              ],
            O.iReturning = O.rCount,
            O.iOnConflict = Nothing
          }
    pure entry
  Tree.writeNotes root entry desc
  redirect $ EntryR $ U.UUID $ entry_id entry

inNamespaceSql :: O.Field O.SqlInt8 -> O.Select (O.Field O.SqlInt8, O.Field O.SqlInt8)
inNamespaceSql namespace = do
  (id_, namespace_, tr_) <- O.selectTable continuantPartOfAtRel
  O.where_ $ namespace_ .== namespace
  pure (id_, tr_)

inNamespaceAllSql :: O.Field O.SqlInt8 -> O.Select (O.Field O.SqlInt8)
inNamespaceAllSql namespace = do
  (id_, tr_) <- inNamespaceSql namespace
  (_, _, all_) <- lookupEntryByNameSql TemporalRegion "All"
  O.where_ $ tr_ .== all_
  pure id_

-- If the mime already exists, use it, otherwise create a new one
getMimeFor :: MonadIO m => Connection -> FilePath -> Entry -> Entity -> Text -> m Int64
getMimeFor conn root entry allEntity mime = do
  mimeEntity <- entry_root <$> lookupEntryByName' conn Namespace "MIME type"
  res <- liftIO $ O.runSelect conn $ do
    (id_, tr_) <- inNamespaceSql $ O.sqlInt8 $ entity_id mimeEntity
    O.where_ $ tr_ .== O.sqlInt8 (entity_id allEntity)
    pure id_
  case res of
    [i] -> pure i
    _ -> newMimeIn conn root entry allEntity mimeEntity mime

newMimeIn :: MonadIO m => Connection -> FilePath -> Entry -> Entity -> Entity -> Text -> m Int64
newMimeIn conn root entry allEntity mimeEntity mime = do
  new <- newEntity conn root entry Identifier Nothing (Just "mime")
  liftIO $
    void $
      O.runInsert conn $
        O.Insert
          { O.iTable = continuantPartOfAtRel,
            O.iRows =
              [ ( O.sqlInt8 new,
                  O.sqlInt8 (entity_id mimeEntity),
                  O.sqlInt8 (entity_id allEntity)
                )
              ],
            O.iReturning = O.rCount,
            O.iOnConflict = Nothing
          }
  liftIO $
    void $
      O.runInsert conn $
        O.Insert
          { O.iTable = identifierRel,
            O.iRows = [(O.sqlInt8 new, O.sqlStrictText mime)],
            O.iReturning = O.rCount,
            O.iOnConflict = Nothing
          }
  pure new
