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
import Korrvigs.Web.Entry.Types
import qualified Korrvigs.Web.Ressources as Rcs
import Network.HTTP.Types (Method, methodGet, methodPost, status500)
import Opaleye ((.==))
import qualified Opaleye as O
import Text.Pandoc.Builder (Blocks)
import Yesod hiding (Entity)

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

type Layout = ([Text], [(String, Blocks)], Text -> Maybe InteractiveWidget)

mkLayout ::
  [String] -> -- widgets to place at the start
  [String] -> -- subsections to place first and in this order
  [String] -> -- subsections to not include
  WidgetMap ->
  Layout
mkLayout prefix place rm mp = (prefixW, headers, mkWidget)
  where
    prefixW = map (T.pack . fst) $ mconcat $ optGet fromRight mp <$> prefix
    headers = placed ++ others
    placed = mconcat $ optGet fromLeft mp <$> place
    others =
      dropKeys (place ++ rm) $
        concatMap (\(k, v) -> maybeToList $ (k,) <$> fromLeft v) $
          M.assocs mp
    mkWidget nm = M.lookup (T.unpack nm) mp >>= fromRight

layout :: Class -> WidgetMap -> Layout
layout Entity = mkLayout [] [] []
layout OntologyClass = mkLayout ["Parent", "Class tree"] ["Generate"] []
layout OntologyRelation = mkLayout [] ["Schema"] []
layout DataFormatSpecification = mkLayout [] ["Identifiers"] []
layout c = layout (isA c)

-- Takes an entry, the id of its root entity and a class, and generate a set of
-- widgets for this class
addWidgets :: Entry -> Class -> Handler WidgetMap
addWidgets entry Entity =
  mconcat
    <$> sequence
      [ Sub.make entry,
        Query.make entry,
        identifiersFor $ entity_id $ entry_root entry
      ]
addWidgets entry OntologyClass =
  mconcat
    <$> sequence
      [ Class.widgetsForClassEntry entry
      ]
-- newInstance method entry

addWidgets entry OntologyRelation = Relation.schemaWidget entry
addWidgets entry Namespace = identifiersIn entry
addWidgets _ _ = pure M.empty

classesPath :: Class -> [Class]
classesPath Entity = [Entity]
classesPath cls = cls : classesPath (isA cls)

build :: Entry -> Handler WidgetMap
build entry = mapM (addWidgets entry) clss <&> M.unions
  where
    clss = classesPath $ entity_class $ entry_root entry

makeEntry :: Method -> (Widget -> Handler TypedContent) -> Entry -> Handler TypedContent
makeEntry method renderer entry = do
  conn <- pgsql
  res <- liftIO $ O.runSelect conn $ do
    (cls_, entry_) <- O.selectTable classEntryTable
    O.where_ $ cls_ .== O.sqlStrictText (name $ entity_class $ entry_root entry)
    return entry_
  case res of
    [clsEntry] -> do
      let root = entry_root entry
      (prefix, widgets, handler) <- layout (entity_class root) <$> build entry
      if method == methodGet
        then do
          widget <- lookupGetParam "widget"
          let response = widget >>= handler <&> snd
          case response of
            Nothing -> do
              note <- noteWidget prefix widgets (fmap fst . handler) entry
              renderer $ Rcs.entryView (entry_name entry) (Just (clsEntry, entity_class root)) note
            Just content -> content
        else
          if method == methodPost
            then do
              widget <- lookupPostParam "widget"
              let response = widget >>= handler <&> snd
              case response of
                Nothing -> invalidArgs ["No widget requested, or invalid widget"]
                Just content -> content
            else badMethod
    _ ->
      sendResponseStatus status500 ("Failed to load class entry" :: Text)

-- Deal with GET request on entry
renderEntry :: (Widget -> Handler TypedContent) -> Entry -> Handler TypedContent
renderEntry = makeEntry methodGet

-- Deal with POST request on entry
processEntry :: (Widget -> Handler TypedContent) -> Entry -> Handler TypedContent
processEntry = makeEntry methodPost
