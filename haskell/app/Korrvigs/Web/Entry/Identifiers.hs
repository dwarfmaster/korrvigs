module Korrvigs.Web.Entry.Identifiers where

import Data.Int (Int64)
import qualified Data.Map as M
import Data.Text (Text)
import Data.UUID (UUID)
import Korrvigs.Classes
import qualified Korrvigs.DB as DB
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Pandoc (renderUrl)
import Korrvigs.Relations
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.DB
import qualified Korrvigs.Web.Entry.Format as Fmt
import Korrvigs.Web.Entry.Types
import qualified Korrvigs.Web.UUID as U
import Opaleye ((.&&), (.==))
import qualified Opaleye as O
import Text.Pandoc.Builder (bulletList, link, para, text)
import Yesod hiding (Entity)

-- Create a new namespace and redirect to it
newNamespace :: Text -> Text -> Handler a
newNamespace nm desc = do
  entry <- createEntry Namespace nm desc
  redirect $ EntryR $ U.UUID $ entry_id entry

identifierValueSql :: O.Field O.SqlInt8 -> O.Select (O.Field O.SqlText)
identifierValueSql ident = do
  (ident_, text_) <- O.selectTable identifierRel
  O.where_ $ ident_ .== ident
  pure text_

-- A widget listing identifiers to an entity
identifiersFor :: Int64 -> Handler WidgetMap
identifiersFor entity = do
  conn <- pgsql
  res <- liftIO $ O.runSelect conn $ do
    (_, _, all_) <- lookupEntryByNameSql TemporalRegion "All"
    -- Find namespace with ident
    (entry_, namespace_, ident_) <- do
      (entry_, namespace_, _) <- O.selectTable entriesTable
      (eid_, _) <- DB.rootFor entry_
      (ident_, tr_) <- Fmt.inNamespaceSql eid_
      O.where_ $ tr_ .== all_
      pure (entry_, namespace_, ident_)
    -- Make sure identifier denotes the current object
    do
      (ident__, target_, tr_) <- O.selectTable denotesAtRel
      O.where_ $ ident_ .== ident__
      O.where_ $ target_ .== O.sqlInt8 entity
      O.where_ $ tr_ .== all_
      pure ()
    -- Find the value of the identifier
    text_ <- identifierValueSql ident_
    pure (entry_, namespace_, text_)
  pure $ case (res :: [(UUID, Text, Text)]) of
    [] -> M.empty
    idents ->
      M.singleton "Identifiers" $
        Left $
          bulletList $
            ( \(entry, nm, ident) ->
                para $
                  link (renderUrl $ EntityRef entry Nothing Nothing) nm (text $ "[" <> nm <> "]")
                    <> text ident
            )
              <$> idents

-- If the given entry is a namespace, create a widget listing all the identifiers
-- in it, with links to
identifiersIn :: Entry -> Handler WidgetMap
identifiersIn namespace = do
  conn <- pgsql
  res <- liftIO $ O.runSelect conn $ do
    (ident_, _) <- Fmt.inNamespaceSql $ O.sqlInt8 $ entity_id $ entry_root namespace
    text_ <- identifierValueSql ident_
    -- Find the entry it identifies, if any
    target_ <- O.limit 1 $ O.optional $ do
      (uuid_, _, _) <- O.selectTable entriesTable
      (root_, _) <- DB.rootFor uuid_
      (ident__, target_, tr_) <- O.selectTable denotesAtRel
      O.where_ $ ident_ .== ident__ .&& target_ .== root_
      (_, _, all_) <- lookupEntryByNameSql TemporalRegion "All"
      O.where_ $ tr_ .== all_
      pure uuid_
    pure (target_, text_)
  pure $ case (res :: [(Maybe UUID, Text)]) of
    [] -> M.empty
    idents ->
      M.singleton
        "Identifiers"
        $ Left
        $ bulletList
        $ ( \(target, ident) -> para $ case target of
              Just tgt -> link (renderUrl $ EntityRef tgt Nothing Nothing) ident $ text ident
              Nothing -> text ident
          )
          <$> idents
