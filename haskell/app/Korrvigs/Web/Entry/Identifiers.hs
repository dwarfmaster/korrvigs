module Korrvigs.Web.Entry.Identifiers where

import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.UUID (UUID)
import Korrvigs.Classes
import qualified Korrvigs.DB as DB
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Relations
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.DB
import qualified Korrvigs.Web.Entry.Format as Fmt
import qualified Korrvigs.Web.UUID as U
import Opaleye ((.==))
import qualified Opaleye as O
import Yesod hiding (Entity)

-- Create a new namespace and redirect to it
newNamespace :: Text -> Text -> Handler a
newNamespace nm desc = do
  entry <- createEntry Namespace nm desc
  redirect $ EntryR $ U.UUID $ entry_id entry

-- A widget listing identifiers to an entity
identifiersFor :: Int64 -> Handler (Map String Widget)
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
    text_ <- do
      (ident__, text_) <- O.selectTable identifierRel
      O.where_ $ ident_ .== ident__
      pure text_
    pure (entry_, namespace_, text_)
  pure $ case (res :: [(UUID, Text, Text)]) of
    [] -> M.empty
    idents ->
      M.singleton
        "Identifiers"
        [whamlet|
        <ul>
          $forall (entry,nm,ident) <- idents
            <li>
              <a href=@{EntryR (U.UUID entry)}>
                [#{nm}]
              #{ident}
      |]
