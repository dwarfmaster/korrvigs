module Korrvigs.Web.Entry.Builder (renderEntry, processEntry) where

import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.Entry.Identifiers (identifiersFor, identifiersIn)
import Korrvigs.Web.Entry.NewInstance (newInstance)
import Korrvigs.Web.Entry.Notes (noteWidget)
import qualified Korrvigs.Web.Entry.OntologyClass as Class
import qualified Korrvigs.Web.Entry.OntologyRelation as Relation
import qualified Korrvigs.Web.Entry.Query as Query
import qualified Korrvigs.Web.Entry.Sub as Sub
import qualified Korrvigs.Web.Ressources as Rcs
import Network.HTTP.Types (Method, methodGet, methodPost)
import Opaleye ((.==))
import qualified Opaleye as O
import Text.Pandoc.Builder (Blocks)
import Yesod (liftIO)

fromLeft :: Either a b -> Maybe a
fromLeft (Left a) = Just a
fromLeft (Right _) = Nothing

fromRight :: Either a b -> Maybe b
fromRight (Left _) = Nothing
fromRight (Right b) = Just b

optGet :: (p -> Maybe a) -> Map String p -> String -> [(String, a)]
optGet f mp key = (key,) <$> maybeToList (M.lookup key mp >>= f)

dropKeys :: [String] -> [(String, a)] -> [(String, a)]
dropKeys keys = filter (\(key, _) -> key `notElem` keys)

type Layout' a b = ([Text], [(String, a)], Text -> Handler (Maybe b))

type Layout = Layout' Blocks Widget

mkLayout ::
  [String] -> -- bs to place at the start
  [String] -> -- as to place first and in this order
  [String] -> -- as to not include
  Map String (Either a b) ->
  Layout' a b
mkLayout prefix place rm mp = (prefixW, headers, mkWidget)
  where
    prefixW = map (T.pack . fst) $ mconcat $ optGet fromRight mp <$> prefix
    headers = placed ++ others
    placed = mconcat $ optGet fromLeft mp <$> place
    others =
      dropKeys (place ++ rm) $
        concatMap (\(k, v) -> maybeToList $ (k,) <$> fromLeft v) $
          M.assocs mp
    mkWidget nm = pure $ M.lookup (T.unpack nm) mp >>= fromRight

layout :: Class -> Map String (Either Blocks Widget) -> Layout
layout Entity = mkLayout [] [] []
layout OntologyClass = mkLayout ["Parent", "Class tree"] ["Generate"] []
layout OntologyRelation = mkLayout [] ["Schema"] []
layout DataFormatSpecification = mkLayout [] ["Identifiers"] []
layout c = layout (isA c)

-- Takes an entry, the id of its root entity and a class, and generate a set of
-- widgets for this class
addWidgets :: Method -> Entry -> Class -> Handler (Map String (Either Blocks Widget))
addWidgets method entry Entity
  | method == methodGet =
      mconcat
        <$> sequence
          []
-- Sub.make entry,
-- Query.make entry,
-- identifiersFor $ entity_id $ entry_root entry

addWidgets method entry OntologyClass =
  mconcat
    <$> sequence
      []
-- Class.widgetsForClassEntry method entry,
-- newInstance method entry

addWidgets method entry OntologyRelation
  | method == methodGet = pure M.empty
-- M.singleton "Schema" <$> Relation.schemaWidget entry
addWidgets method entry Namespace | method == methodGet = pure M.empty
-- identifiersIn entry
addWidgets _ _ _ = pure M.empty

classesPath :: Class -> [Class]
classesPath Entity = [Entity]
classesPath cls = cls : classesPath (isA cls)

build :: Method -> Entry -> Handler (Map String (Either Blocks Widget))
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
      (prefix, widgets, handler) <- layout (entity_class root) <$> build method entry
      note <- noteWidget prefix widgets handler entry
      pure $ Rcs.entryView (entry_name entry) Nothing (Just (clsEntry, entity_class root)) note
    _ ->
      pure $ Rcs.entryView (entry_name entry) (Just "Failed to load root entry") Nothing (pure ())

-- Deal with GET request on entry
renderEntry :: Entry -> Handler Widget
renderEntry = makeEntry methodGet

-- Deal with POST request on entry
processEntry :: Entry -> Handler Widget
processEntry = makeEntry methodPost
