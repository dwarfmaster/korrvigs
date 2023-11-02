module Korrvigs.Web.Entry.Builder (renderEntry) where

import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Opaleye ((.&&), (.==))
import qualified Opaleye as O
import Text.Hamlet (hamlet)
import Yesod (liftIO, toWidget, whamlet)

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
layout c = layout (isA c)

-- Takes an entry, the id of its root entity and a class, and generate a set of
-- widgets for this class
addWidgets :: Entry -> Int64 -> Class -> Handler (Map String Widget)
addWidgets _ _ _ = pure $ M.empty

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
    Nothing -> pure $ err "Failed to load root entry"
    Just root -> do
      widgets <- layout (entity_class root) <$> build entry root
      -- TODO add link to class page
      pure $ do
        toWidget
          [hamlet|
          <h1> #{entry_name entry}
          <div .entry-class> #{name (entity_class root)}
        |]
        mconcat $ wrapFragment <$> widgets
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
    err :: Text -> Widget
    err msg =
      [whamlet|
    <h1> #{entry_name entry}
    <fieldset #error-field>
      <legend> Error
      <div> #{msg}
  |]
    wrapFragment :: (String, Widget) -> Widget
    wrapFragment (nm, widget) =
      [whamlet|
    <fieldset .fragment>
      <legend> #{nm}
      <div .fragment-body>
        ^{widget}
  |]
