module Korrvigs.Web.Entry.Query where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Korrvigs.Definition
import Korrvigs.Pandoc (renderUrl)
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.DB ((.==?))
import Opaleye ((.==))
import qualified Opaleye as O
import Text.Pandoc.Builder (Blocks, bulletList, link, para, text)
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

make :: Entry -> Handler (Map String (Either Blocks Widget))
make entry = build root =<< listQueries root
  where
    root = entry_root entry

mkList :: Entity -> [Text] -> Blocks
mkList entity queries =
  bulletList $ (\query -> para $ link (mkUrl query) query $ text query) <$> queries
  where
    mkUrl query = renderUrl $ EntityRef (entity_uuid entity) (entity_sub entity) (Just query)

build :: Entity -> [Text] -> Handler (Map String (Either Blocks Widget))
build _ [] = pure M.empty
build entity queries =
  pure $ M.singleton "Queries" $ Left $ mkList entity queries
