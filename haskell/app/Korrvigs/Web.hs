{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Web (Korrvigs (..)) where

import Control.Monad ((>=>))
import Data.Text (Text)
import qualified Korrvigs.Classes as Cls
import Korrvigs.Definition
import qualified Korrvigs.Relations as Rels
import Korrvigs.Web.Backend
import Korrvigs.Web.Backend ()
import Korrvigs.Web.DB
import Korrvigs.Web.Entry (getAllEntriesR, processEntry, renderEntry)
import Korrvigs.Web.Entry.Notes (noteEditor)
import Korrvigs.Web.Header
import Korrvigs.Web.Method
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

getEntryEditR :: UUID -> Handler TypedContent
getEntryEditR (UUID uuid) = noteEditor methodGet uuid defaultLayout

postEntryEditR :: UUID -> Handler TypedContent
postEntryEditR (UUID uuid) = noteEditor methodPost uuid defaultLayout

getEntryQueryR :: UUID -> Text -> Handler Html
getEntryQueryR (UUID uuid) query = generateForEntity $ EntityRef uuid Nothing (Just query)

getEntrySubR :: UUID -> Text -> Handler Html
getEntrySubR (UUID uuid) sub = generateForEntity $ EntityRef uuid (Just sub) Nothing

getEntrySubQueryR :: UUID -> Text -> Text -> Handler Html
getEntrySubQueryR (UUID uuid) sub query = generateForEntity $ EntityRef uuid (Just sub) (Just query)

generateForEntity :: EntityRef -> Handler Html
generateForEntity ref =
  findEntity ref >>= \case
    Nothing -> notFound
    Just ent -> defaultLayout [whamlet|Page for [#{Cls.name (entity_class ent)}] #{show ref}|]

getGenerateClassesR :: Handler Text
getGenerateClassesR = pgsql >>= Cls.generateClassHs

getGenerateRelsSqlR :: Handler Text
getGenerateRelsSqlR = do
  conn <- pgsql
  root <- korrRoot
  Rels.generateRelationsSql conn root

getGenerateRelsHsR :: Handler Text
getGenerateRelsHsR = do
  conn <- pgsql
  root <- korrRoot
  Rels.generateRelationsHs conn root
