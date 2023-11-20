module Korrvigs.Web.Entry.Builder (renderEntry, processEntry) where

import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.Entry.Identifiers (identifiersFor, identifiersIn)
import Korrvigs.Web.Entry.NewInstance (newInstance)
import Korrvigs.Web.Entry.Notes
import qualified Korrvigs.Web.Entry.OntologyClass as Class
import qualified Korrvigs.Web.Entry.OntologyRelation as Relation
import qualified Korrvigs.Web.Entry.Sub as Sub
import qualified Korrvigs.Web.Ressources as Rcs
import Network.HTTP.Types (Method, methodGet, methodPost)
import Opaleye ((.==))
import qualified Opaleye as O
import Yesod (liftIO)

optGet :: Map String a -> String -> [(String, a)]
optGet mp key = (key,) <$> maybeToList (M.lookup key mp)

dropKeys :: [String] -> [(String, a)] -> [(String, a)]
dropKeys keys = filter (\(key, _) -> key `notElem` keys)

mkLayout :: [String] -> [String] -> Map String a -> [(String, Bool, a)]
mkLayout place rm mp = placed ++ others
  where
    placed = map (\(nm, v) -> (nm, True, v)) $ mconcat $ optGet mp <$> place
    others = map (\(nm, v) -> (nm, False, v)) $ dropKeys (place ++ rm) $ M.assocs mp

layout :: Class -> Map String Widget -> [(String, Bool, Widget)]
layout Entity = mkLayout ["Notes"] []
layout OntologyClass = mkLayout ["Parent", "Class tree", "Generate", "Notes"] []
layout OntologyRelation = mkLayout ["Schema", "Notes"] []
layout DataFormatSpecification = mkLayout ["Identifiers", "Notes"] []
layout c = layout (isA c)

-- Takes an entry, the id of its root entity and a class, and generate a set of
-- widgets for this class
addWidgets :: Method -> Entry -> Class -> Handler (Map String Widget)
addWidgets method entry Entity
  | method == methodGet =
      mconcat
        <$> sequence
          [ M.singleton "Notes" <$> noteWidget entry,
            Sub.make entry,
            identifiersFor $ entity_id $ entry_root entry
          ]
addWidgets method entry OntologyClass =
  mconcat
    <$> sequence
      [ Class.widgetsForClassEntry method entry,
        newInstance method entry
      ]
addWidgets method entry OntologyRelation
  | method == methodGet =
      M.singleton "Schema" <$> Relation.schemaWidget entry
addWidgets method entry Namespace | method == methodGet = identifiersIn entry
addWidgets _ _ _ = pure M.empty

classesPath :: Class -> [Class]
classesPath Entity = [Entity]
classesPath cls = cls : classesPath (isA cls)

build :: Method -> Entry -> Handler (Map String Widget)
build method entry = mapM (addWidgets method entry) clss <&> M.unions
  where
    clss = classesPath $ entity_class $ entry_root entry

makeEntry :: Method -> Entry -> Handler Widget
makeEntry method entry = do
  conn <- pgsql
  res <- liftIO $ O.runSelect conn $ do
    (cls_, entry_) <- O.selectTable classEntryTable
    O.where_ $ cls_ .== O.sqlStrictText (name $ entity_class $ entry_root entry)
    return entry_
  case res of
    [clsEntry] -> do
      let root = entry_root entry
      widgets <- layout (entity_class root) <$> build method entry
      pure $ Rcs.entryView (entry_name entry) Nothing (Just (clsEntry, entity_class root)) widgets
    _ ->
      pure $ Rcs.entryView (entry_name entry) (Just "Failed to load root entry") Nothing []

-- Deal with GET request on entry
renderEntry :: Entry -> Handler Widget
renderEntry = makeEntry methodGet

-- Deal with POST request on entry
processEntry :: Entry -> Handler Widget
processEntry = makeEntry methodPost
