module Korrvigs.Web.Entry (getEntryR, postEntryR) where

import Control.Lens hiding (children)
import Control.Monad
import qualified Data.Aeson.Encoding as VEnc
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Enc
import Data.Time.Format.ISO8601 (iso8601Show)
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.File as File
import qualified Korrvigs.Web.Entry.Link as Link
import Korrvigs.Web.Leaflet
import Korrvigs.Web.Login
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import Opaleye hiding (null)
import Yesod

-- An entry page is constitued of the following parts
-- The entry title (if any) + the entry name
-- A link to download the entry
-- The metadata in a foldable blocks, with edition
-- The backlinks,parents and subs
-- The entry content

titleWidget :: Entry -> Handler Widget
titleWidget entry = do
  title :: Maybe Text <- fmap join $ rSelectOne $ do
    mtdt <- selectTable entriesMetadataTable
    where_ $ (mtdt ^. sqlEntry) .== sqlId (entry ^. name)
    where_ $ mtdt ^. sqlKey .== sqlStrictText "title"
    pure $ sqlJsonToText $ toNullable $ mtdt ^. sqlValue
  pure
    [whamlet|
    ^{htmlKind $ entry ^. kind}
    <span .download-button>
      <a href=@{EntryDownloadR $ WId $ entry ^. name}>
        ⬇
    <h1>
      $maybe t <- title
        #{t}
      <span .entry-name>
        (#{unId $ entry ^. name})
  |]

-- TODO make link to day viewer
dateWidget :: Entry -> Handler Widget
dateWidget entry = case entry ^. date of
  Just time ->
    pure
      [whamlet|
      <a href="">
        #{iso8601Show time}
    |]
  Nothing -> pure mempty

geometryWidget :: Entry -> Handler Widget
geometryWidget entry = case entry ^. geo of
  Nothing -> pure mempty
  Just geometry -> do
    detClass <- newIdent
    pure $ do
      [whamlet|
        <details .#{detClass}>
          <summary>Geometry
          ^{leafletWidget "map" [MapItem geometry Nothing]}
      |]
      toWidget
        [julius|
        var details = document.querySelector(#{"." <> detClass})
        details.addEventListener("toggle", function() {
          map.invalidateSize()
        })
      |]

mtdtWidget :: Entry -> Handler Widget
mtdtWidget entry = do
  pure
    [whamlet|
  <details>
    <summary>Metadata
    <table>
      <tr>
        <th>Key
        <th>Value
        <th>Read Only
      $forall (key,val) <- M.toList (entry ^. metadata)
        <tr>
          <th>#{key}
          <th>
            #{prepareMtdtValue $ val ^. metaValue}
          <th>
            $if val ^. metaReadOnly
              True
            $else
              False
  |]
  where
    prepareMtdtValue :: Value -> LT.Text
    prepareMtdtValue val =
      let txt = Enc.decodeUtf8 $ VEnc.encodingToLazyByteString $ VEnc.value val
       in if LT.length txt < 50
            then txt
            else LT.take 47 txt <> "..."

refsWidget :: Entry -> Handler Widget
refsWidget entry = do
  parents :: [Id] <- rSelect $ selectTargetsFor entriesSubTable $ entry ^. name
  children :: [Id] <- rSelect $ selectSourcesFor entriesSubTable $ entry ^. name
  backrefs :: [Id] <- rSelect $ selectSourcesFor entriesRefTable $ entry ^. name
  if null parents && null children && null backrefs
    then pure mempty
    else
      pure
        [whamlet|
        <details .refs>
          <summary>References
          <div .refsrow>
            <div .refscol>
              <h4> Parents
              <ul>
                $forall par <- parents
                  <li>
                    <a href=@{EntryR $ WId par}>#{unId par}
            <div .refscol>
              <h4> Childrent
              <ul>
                $forall ch <- children
                  <li>
                    <a href=@{EntryR $ WId ch}>#{unId ch}
            <div .refscol>
              <h4> Backreferences
              <ul>
                $forall back <- backrefs
                  <li>
                    <a href=@{EntryR $ WId back}>#{unId back}
      |]

contentWidget :: Entry -> Handler Widget
contentWidget entry = case entry ^. kindData of
  LinkD link -> Link.content link
  NoteD note -> undefined
  FileD file -> File.content file

entryWidget :: Entry -> Handler Widget
entryWidget entry = do
  title <- titleWidget entry
  dt <- dateWidget entry
  geom <- geometryWidget entry
  mtdt <- mtdtWidget entry
  refs <- refsWidget entry
  content <- contentWidget entry
  pure $ do
    Rcs.entryStyle
    title
    dt
    geom
    mtdt
    refs
    content

getEntryR :: WebId -> Handler Html
getEntryR (WId i) =
  load i >>= \case
    Just entry -> entryWidget entry >>= logWrap . defaultLayout
    Nothing -> notFound

postEntryR :: WebId -> Handler Html
postEntryR (WId _) = logWrap undefined
