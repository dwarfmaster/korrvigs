{-# LANGUAGE LambdaCase #-}

module Korrvigs.Web.Entry.Sub where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Schema
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.UUID as U
import Opaleye ((.&&), (.==))
import qualified Opaleye as O
import Yesod hiding (Entity)

make :: Entry -> Handler (Map String Widget)
make entry = do
  conn <- pgsql
  subs <- liftIO $ O.runSelect conn $ do
    (_, cls_, uuid_, sub_, query_) <- O.selectTable entitiesTable
    O.where_ $ uuid_ .== O.sqlUUID (entry_id entry)
    O.where_ $ O.isNull query_ .&& cls_ .== O.sqlStrictText (name File)
    pure sub_
  build entry $ catMaybes subs

build :: Entry -> [Text] -> Handler (Map String Widget)
build _ [] = pure M.empty
build entry subs =
  pure $
    M.singleton
      "Subs"
      [whamlet|
    <ul>
      $forall sub <- subs
        <li>
          <a href=@{EntrySubR (U.UUID (entry_id entry)) sub}>
            #{sub}
  |]
