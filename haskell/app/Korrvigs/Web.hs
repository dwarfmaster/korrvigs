{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Web (Korrvigs (..)) where

import Data.Text (Text)
import qualified Korrvigs.Classes as Cls
import Korrvigs.Definition
import Korrvigs.Web.Backend
import Korrvigs.Web.Backend ()
import Korrvigs.Web.DB
import qualified Korrvigs.Web.Entry as Entries
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.UUID (UUID (UUID))
import Yesod

mkYesodDispatch "Korrvigs" korrvigsRoutes

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello from korrvigs!|]

getAllEntriesR :: Handler TypedContent
getAllEntriesR = selectRep $ do
  provideRep Entries.allEntriesJSON
  provideRep $ defaultLayout Rcs.entrySelect

getEntryR :: UUID -> Handler Html
getEntryR (UUID uuid) = generateForEntity $ EntityRef uuid Nothing Nothing

getEntryQueryR :: UUID -> Text -> Handler Html
getEntryQueryR (UUID uuid) query = generateForEntity $ EntityRef uuid Nothing (Just query)

getEntitySubR :: UUID -> Text -> Handler Html
getEntitySubR (UUID uuid) sub = generateForEntity $ EntityRef uuid (Just sub) Nothing

getEntitySubQueryR :: UUID -> Text -> Text -> Handler Html
getEntitySubQueryR (UUID uuid) sub query = generateForEntity $ EntityRef uuid (Just sub) (Just query)

generateForEntity :: EntityRef -> Handler Html
generateForEntity ref =
  findEntity ref >>= \case
    Nothing -> notFound
    Just ent -> defaultLayout [whamlet|Page for [#{Cls.name (entity_class ent)}] #{show ref}|]
