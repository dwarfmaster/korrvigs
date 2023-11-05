{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Web (Korrvigs (..)) where

import Control.Monad ((>=>))
import Data.Text (Text)
import qualified Korrvigs.Classes as Cls
import Korrvigs.Definition
import Korrvigs.Web.Backend
import Korrvigs.Web.Backend ()
import Korrvigs.Web.DB
import Korrvigs.Web.Entry (getAllEntriesR, processEntry, renderEntry)
import Korrvigs.Web.Header
import Korrvigs.Web.Routes
import Korrvigs.Web.UUID (UUID (UUID))
import Yesod

mkYesodDispatch "Korrvigs" korrvigsRoutes

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  header HSHome
  [whamlet|Hello from korrvigs!|]

getEntryR :: UUID -> Handler Html
getEntryR (UUID uuid) =
  findEntry uuid
    >>= maybe notFound (renderEntry >=> addHeaderM HSEntries >=> defaultLayout)

postEntryR :: UUID -> Handler Html
postEntryR (UUID uuid) = findEntry uuid >>= maybe notFound (processEntry >=> defaultLayout)

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

getGenerateR :: Handler Text
getGenerateR = pgsql >>= Cls.generateClassHs
