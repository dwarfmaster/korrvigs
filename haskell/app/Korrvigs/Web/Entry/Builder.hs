module Korrvigs.Web.Entry.Builder (renderEntry, processEntry) where

import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.UUID (UUID)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.Entry.Notes
import qualified Korrvigs.Web.Entry.OntologyClass as Class
import qualified Korrvigs.Web.Ressources as Rcs
import Network.HTTP.Types (Method, methodGet, methodPost)
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
layout OntologyClass = mkLayout ["Parent", "Class tree", "Generate", "Notes"] []
layout c = layout (isA c)

-- Takes an entry, the id of its root entity and a class, and generate a set of
-- widgets for this class
addWidgets :: Method -> Entry -> Int64 -> Class -> Handler (Map String Widget)
addWidgets method entry _ Entity
  | method == methodGet =
      M.singleton "Notes" <$> noteWidget entry
addWidgets method entry _ OntologyClass =
  Class.widgetsForClassEntry method entry
addWidgets _ _ _ _ = pure M.empty

classesPath :: Class -> [Class]
classesPath Entity = [Entity]
classesPath cls = cls : classesPath (isA cls)

-- Takes an entry and its root entity
build :: Method -> Entry -> Entity -> Handler (Map String Widget)
build method entry root = mapM (addWidgets method entry $ entity_id root) clss >>= pure . M.unions
  where
    clss = classesPath $ entity_class root

makeEntry :: Method -> Entry -> Handler Widget
makeEntry method entry = do
  conn <- pgsql
  res <- liftIO $ mkEntity <$> O.runSelect conn sql
  case res of
    Nothing ->
      pure $ Rcs.entryView (entry_name entry) (Just "Failed to load root entry") Nothing []
    Just (clsEntry, root) -> do
      widgets <- layout (entity_class root) <$> build method entry root
      pure $ Rcs.entryView (entry_name entry) Nothing (Just $ (clsEntry, entity_class root)) widgets
  where
    sql :: O.Select (O.Field O.SqlInt8, O.Field O.SqlText, O.Field O.SqlUuid)
    sql = do
      (i_, class_, uuid_, sub_, query_) <- O.selectTable entitiesTable
      (cls_, entry_) <- O.selectTable classEntryTable
      O.where_ $
        (uuid_ .== O.sqlUUID (entry_id entry))
          .&& (O.isNull sub_)
          .&& (O.isNull query_)
          .&& (cls_ .== class_)
      return (i_, class_, entry_)
    mkEntity :: [(Int64, Text, UUID)] -> Maybe (UUID, Entity)
    mkEntity [(i, cls, uuid)] =
      parse cls >>= \c -> Just (uuid, MkEntity i c (entry_id entry) Nothing Nothing)
    mkEntity _ = Nothing

-- Deal with GET request on entry
renderEntry :: Entry -> Handler Widget
renderEntry = makeEntry methodGet

-- Deal with POST request on entry
processEntry :: Entry -> Handler Widget
processEntry = makeEntry methodPost
