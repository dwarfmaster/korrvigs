module Korrvigs.Web.Entry.Query where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Korrvigs.Definition
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.DB ((.==?))
import qualified Korrvigs.Web.UUID as U
import Opaleye ((.==))
import qualified Opaleye as O
import Yesod hiding (Entity)

listQueries :: Entity -> Handler [Text]
listQueries entity = do
  conn <- pgsql
  queries <- liftIO $ O.runSelect conn $ do
    (_, _, uuid_, sub_, query_) <- O.selectTable entitiesTable
    O.where_ $ sub_ .==? (O.sqlStrictText <$> entity_sub entity)
    O.where_ $ uuid_ .== O.sqlUUID (entity_uuid entity)
    pure query_
  pure $ catMaybes queries

make :: Entry -> Handler (Map String Widget)
make entry = build root =<< listQueries root
  where
    root = entry_root entry

mkList :: Entity -> [Text] -> Widget
mkList entity queries =
  [whamlet|
<ul>
  $forall query <- queries
    <li>
      <a href=@{mkUrl query}>
        #{query}
 |]
  where
    mkUrl query = case entity_sub entity of
      Just sub -> EntrySubQueryR (U.UUID (entity_uuid entity)) sub query
      Nothing -> EntryQueryR (U.UUID (entity_uuid entity)) query

build :: Entity -> [Text] -> Handler (Map String Widget)
build _ [] = pure M.empty
build entity queries =
  pure $ M.singleton "Queries" $ mkList entity queries
