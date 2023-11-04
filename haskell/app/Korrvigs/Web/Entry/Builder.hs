module Korrvigs.Web.Entry.Builder (renderEntry) where

import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.Entry.Notes
import qualified Korrvigs.Web.Entry.OntologyClass as Class
import qualified Korrvigs.Web.Ressources as Rcs
import Opaleye ((.&&), (.==))
import qualified Opaleye as O
import Yesod (liftIO)

optGet :: Map String a -> String -> [(String, a)]
optGet mp key = (key,) <$> (maybe [] (: []) $ M.lookup key mp)

dropKeys :: [String] -> [(String, a)] -> [(String, a)]
dropKeys keys set = filter (\(key, _) -> not $ elem key keys) set

mkLayout :: [String] -> [String] -> Map String a -> [(String, a)]
mkLayout place rm mp = placed ++ others
  where
    placed = mconcat $ optGet mp <$> place
    others = dropKeys (place ++ rm) $ M.assocs mp

layout :: Class -> Map String Widget -> [(String, Widget)]
layout Entity = mkLayout ["Notes"] []
layout OntologyClass = mkLayout ["Class tree", "Notes"] []
layout c = layout (isA c)

-- Takes an entry, the id of its root entity and a class, and generate a set of
-- widgets for this class
addWidgets :: Entry -> Int64 -> Class -> Handler (Map String Widget)
addWidgets entry _ Entity = M.singleton "Notes" <$> noteWidget entry
addWidgets entry _ OntologyClass = Class.widgetsForClassEntry entry
addWidgets _ _ _ = pure M.empty

classesPath :: Class -> [Class]
classesPath Entity = [Entity]
classesPath cls = cls : classesPath (isA cls)

-- Takes an entry and its root entity
build :: Entry -> Entity -> Handler (Map String Widget)
build entry root = mapM (addWidgets entry $ entity_id root) clss >>= pure . M.unions
  where
    clss = classesPath $ entity_class root

renderEntry :: Entry -> Handler Widget
renderEntry entry = do
  conn <- pgsql
  res <- liftIO $ mkEntity <$> O.runSelect conn sql
  case res of
    Nothing ->
      pure $ Rcs.entryView (entry_name entry) (Just "Failed to load root entry") Nothing []
    Just root -> do
      widgets <- layout (entity_class root) <$> build entry root
      pure $ Rcs.entryView (entry_name entry) Nothing (Just $ entity_class root) widgets
  where
    sql :: O.Select (O.Field O.SqlInt8, O.Field O.SqlText)
    sql = do
      (i_, class_, uuid_, sub_, query_) <- O.selectTable entitiesTable
      O.where_ $
        (uuid_ .== O.sqlUUID (entry_id entry))
          .&& (O.isNull sub_)
          .&& (O.isNull query_)
      return (i_, class_)
    mkEntity :: [(Int64, Text)] -> Maybe Entity
    mkEntity [(i, cls)] =
      parse cls >>= \c -> Just $ MkEntity i c (entry_id entry) Nothing Nothing
    mkEntity _ = Nothing
