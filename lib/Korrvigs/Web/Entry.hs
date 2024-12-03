module Korrvigs.Web.Entry (getEntryR, postEntryR) where

import Control.Lens hiding (children)
import Control.Monad
import qualified Data.Aeson.Encoding as VEnc
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Enc
import Data.Time.Format.ISO8601 (iso8601Show)
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils.Base16
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Event as Event
import qualified Korrvigs.Web.Entry.File as File
import qualified Korrvigs.Web.Entry.Link as Link
import qualified Korrvigs.Web.Entry.Note as Note
import Korrvigs.Web.Leaflet
import Korrvigs.Web.Login
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import qualified Korrvigs.Web.Vis.Network as Network
import Opaleye hiding (groupBy, null)
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
          ^{leafletWidget "map" [MapItem geometry Nothing Nothing]}
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
  subs :: [(EntryRow, EntryRow)] <- rSelect $ selectBidir entriesSubTable
  refs :: [(EntryRow, EntryRow)] <- rSelect $ selectBidir entriesRefTable
  let rows =
        (view _1 <$> subs)
          ++ (view _2 <$> subs)
          ++ (view _1 <$> refs)
          ++ (view _2 <$> refs)
  let entries = map head $ groupBy (\r1 r2 -> cmp r1 r2 == EQ) $ sortBy cmp rows
  nodes <- mapM mkNode entries
  edgeStyle <- Network.defEdgeStyle
  base <- getBase
  let subStyle = edgeStyle & Network.edgeColor .~ base edgeSubColor
  let refStyle = edgeStyle & Network.edgeColor .~ base edgeRefColor
  let edges = (mkEdge subStyle <$> subs) ++ (mkEdge refStyle <$> refs)
  if null entries
    then pure mempty
    else do
      network <- Network.network "network" nodes edges
      detId <- newIdent
      pure $ do
        [whamlet|
          <details ##{detId}>
            <summary>Network
            ^{network}
        |]
        toWidget
          [julius|
          document.getElementById(#{detId}).addEventListener("toggle", function(e) {
            if (e.newState == "open") {
              network.fit()
            }
          })
        |]
  where
    cmp :: EntryRow -> EntryRow -> Ordering
    cmp row1 row2 = compare (row1 ^. sqlEntryName) (row2 ^. sqlEntryName)
    selectBidir :: Table a RelRowSQL -> Select (EntryRowSQL, EntryRowSQL)
    selectBidir tbl = do
      sub <- selectTable tbl
      where_ $
        (sub ^. source)
          .== sqlId (entry ^. name)
          .|| (sub ^. target)
          .== sqlId (entry ^. name)
      src <- selectTable entriesTable
      where_ $ (sub ^. source) .== (src ^. sqlEntryName)
      tgt <- selectTable entriesTable
      where_ $ (sub ^. target) .== (tgt ^. sqlEntryName)
      pure (src, tgt)
    mkNode :: EntryRow -> Handler (Text, Text, Network.NodeStyle)
    mkNode e = do
      style <- Network.defNodeStyle
      render <- getUrlRender
      base <- getBase
      let color =
            if e ^. sqlEntryName == entry ^. name
              then base Base07
              else base $ colorKind $ e ^. sqlEntryKind
      pure
        ( unId $ e ^. sqlEntryName,
          "@" <> unId (e ^. sqlEntryName),
          style
            & Network.nodeBorder .~ color
            & Network.nodeSelected .~ color
            & Network.nodeLink ?~ render (EntryR $ WId $ e ^. sqlEntryName)
        )
    mkEdge :: Network.EdgeStyle -> (EntryRow, EntryRow) -> (Text, Text, Network.EdgeStyle)
    mkEdge style (r1, r2) = (unId $ r1 ^. sqlEntryName, unId $ r2 ^. sqlEntryName, style)

contentWidget :: Entry -> Handler Widget
contentWidget entry = case entry ^. kindData of
  LinkD link -> Link.content link
  NoteD note -> Note.content note
  FileD file -> File.content file
  EventD event -> Event.content event

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
