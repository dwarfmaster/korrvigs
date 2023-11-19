module Korrvigs.Web.Sub (widget) where

import Control.Monad (join)
import Data.Text (Text)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Relations (denotesAtRel, identifierRel)
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.Entry.Format (inNamespaceSql)
import Opaleye ((.&&), (.==))
import qualified Opaleye as O
import Yesod

widgetForMime :: Text -> Handler (Maybe Widget)
widgetForMime _ = pure Nothing

findMimeTypeSql :: Entry -> FilePath -> O.Select (O.Field O.SqlText)
findMimeTypeSql entry sub = do
  -- Find the entity
  (id_, _, entry_, sub_, query_) <- O.selectTable entitiesTable
  O.where_ $ entry_ .== O.sqlUUID (entry_id entry)
  O.where_ $ O.matchNullable (O.sqlBool False) (.== O.sqlString sub) sub_
  O.where_ $ O.isNull query_
  -- Find its mime identifier
  (_, _, all_) <- lookupEntryByNameSql TemporalRegion "All"
  (_, _, mimeNamespace_) <- lookupEntryByNameSql Namespace "MIME type"
  (mime_, tr1_) <- inNamespaceSql mimeNamespace_
  (mime__, denoted_, tr2_) <- O.selectTable denotesAtRel
  O.where_ $ mime_ .== mime__ .&& denoted_ .== id_
  O.where_ $ tr1_ .== all_ .&& tr2_ .== all_
  -- Find the text of the identifier
  (identifier_, text_) <- O.selectTable identifierRel
  O.where_ $ identifier_ .== mime_
  pure text_

findMimeType :: Entry -> FilePath -> Handler (Maybe Text)
findMimeType entry sub = do
  conn <- pgsql
  res <- liftIO $ O.runSelect conn $ findMimeTypeSql entry sub
  pure $ case res of
    [mime] -> Just mime
    _ -> Nothing

widget :: Entry -> FilePath -> Handler (Maybe Widget)
widget entry sub = findMimeType entry sub >>= (fmap join . mapM widgetForMime)
