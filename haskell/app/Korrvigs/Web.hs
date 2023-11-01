{-# LANGUAGE LambdaCase #-}

module Korrvigs.Web where

import Data.Text (Text)
import qualified Korrvigs.Classes as Cls
import Korrvigs.Definition
import Korrvigs.Web.Backend
import Korrvigs.Web.DB
import Korrvigs.Web.UUID (UUID (UUID))
import Yesod

mkYesod
  "Korrvigs"
  [parseRoutes|
/ HomeR GET
/entity/#UUID EntityR GET
/entity/#UUID/#Text EntityQueryR GET
/entry/#UUID/#Text EntrySubR GET
/entry/#UUID/#Text/#Text EntrySubQueryR GET
|]

instance Yesod Korrvigs

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello from korrvigs!|]

getEntityR :: UUID -> Handler Html
getEntityR (UUID uuid) = generateForEntity $ EntityRef uuid Nothing Nothing

getEntityQueryR :: UUID -> Text -> Handler Html
getEntityQueryR (UUID uuid) query = generateForEntity $ EntityRef uuid Nothing (Just query)

getEntrySubR :: UUID -> Text -> Handler Html
getEntrySubR (UUID uuid) sub = generateForEntity $ EntityRef uuid (Just sub) Nothing

getEntrySubQueryR :: UUID -> Text -> Text -> Handler Html
getEntrySubQueryR (UUID uuid) sub query = generateForEntity $ EntityRef uuid (Just sub) (Just query)

generateForEntity :: EntityRef -> Handler Html
generateForEntity ref =
  findEntity ref >>= \case
    Nothing -> notFound
    Just ent -> defaultLayout [whamlet|Page for [#{Cls.name (entity_class ent)}] #{show ref}|]
