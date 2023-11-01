module Korrvigs.Web.Entry.All (allEntriesJSON) where

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.UUID as U
import Korrvigs.Schema
import Korrvigs.Web.Backend
import qualified Opaleye as O
import Yesod (liftIO)

entryToJson :: U.UUID -> Text -> JSON.Value
entryToJson i name =
  JSON.object
    [ "id" .= U.toText i,
      "name" .= name
    ]

allEntriesAsJSON :: Handler [JSON.Value]
allEntriesAsJSON = do
  conn <- pgsql
  res <- liftIO $ O.runSelect conn sql
  pure $ uncurry entryToJson <$> res
  where
    sql :: O.Select (O.Field O.SqlUuid, O.Field O.SqlText)
    sql = do
      (i_, name_, _) <- O.selectTable entriesTable
      return (i_, name_)

allEntriesJSON :: Handler JSON.Value
allEntriesJSON = JSON.toJSONList <$> allEntriesAsJSON
