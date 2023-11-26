module Korrvigs.Web.Query where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.UUID (UUID)
import Korrvigs.Classes
import Korrvigs.Classes.Colors
import Korrvigs.DB (rootFor)
import Korrvigs.Definition
import Korrvigs.Relations
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.DB (findEntry)
import Korrvigs.Web.Entry.Format (inNamespaceAllSql)
import qualified Korrvigs.Web.UUID as U
import Opaleye ((.==))
import qualified Opaleye as O
import Yesod hiding (Entity)

extra :: Class -> Entry -> Entity -> Handler Widget
extra Identifier _ entity = do
  conn <- pgsql
  value' <-
    listToMaybe
      <$> liftIO
        ( O.runSelect conn $ do
            (id_, value_) <- O.selectTable identifierRel
            O.where_ $ id_ .== O.sqlInt8 (entity_id entity)
            pure value_
        )
  let value = value' :: Maybe Text
  let noValue = "<no value found>" :: Text
  namespace' <-
    listToMaybe
      <$> liftIO
        ( O.runSelect conn $ do
            (uuid_, name_, _) <- O.selectTable entriesTable
            (nm_, cls_) <- rootFor uuid_
            O.where_ $ cls_ .== O.sqlStrictText (name Namespace)
            id_ <- inNamespaceAllSql nm_
            O.where_ $ id_ .== O.sqlInt8 (entity_id entity)
            pure (uuid_, name_)
        )
  let namespace = namespace' :: Maybe (UUID, Text)
  let noNamespace = "<no namespace found>" :: Text
  pure
    [whamlet|
      <p>
        $maybe (uuid,nm) <- namespace
          <a href=@{EntryR (U.UUID uuid)}>
            #{nm}
        $nothing
          #{noNamespace}
        <span>
          ::
        $maybe value <- value
          #{value}
        $nothing
          #{noValue}
    |]
extra _ _ _ = pure $ pure ()

mkTitle :: Entry -> Maybe Text -> Maybe Text -> Widget
mkTitle entry sub query =
  [whamlet|
    <h1>
      <a href=@{EntryR uuid}>
        #{entry_name entry}
      $maybe sub <- sub
        <span .sub>
          <a href=@{EntrySubR uuid sub}>
            #{sub}
      $maybe query <- query
        <span .query>
          #{query}
  |]
  where
    uuid = U.UUID $ entry_id entry

widget :: Entity -> Handler Widget
widget entity = do
  entry <- maybe notFound pure =<< findEntry (entity_uuid entity)
  let title = mkTitle entry (entity_sub entity) (entity_query entity)
  let cls = entity_class entity
  let clsColor = "--base" ++ classBase cls
  conn <- pgsql
  clsUuid <-
    listToMaybe
      <$> liftIO
        ( O.runSelect conn $ do
            (cls_, entry_) <- O.selectTable classEntryTable
            O.where_ $ cls_ .== O.sqlStrictText (name cls)
            pure entry_
        )
  wdgt <- extra cls entry entity
  pure
    [whamlet|
      $maybe uuid <- clsUuid
        <div .entry-class style="background-color: var(#{clsColor})">
          <a href=@{EntryR (U.UUID uuid)}>
            #{name (entity_class entity)}
      ^{title}
      ^{wdgt}
    |]
