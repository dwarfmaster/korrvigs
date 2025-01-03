module Korrvigs.Web.Entry (getEntryR, postEntryR) where

import Control.Lens hiding (children)
import Control.Monad
import Data.List
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601Show)
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Note.Loc (SubLoc (SubLoc))
import Korrvigs.Utils.Base16
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Event as Event
import qualified Korrvigs.Web.Entry.File as File
import qualified Korrvigs.Web.Entry.Link as Link
import qualified Korrvigs.Web.Entry.Metadata as Mtdt
import qualified Korrvigs.Web.Entry.Note as Note
import qualified Korrvigs.Web.Home as Home
import Korrvigs.Web.Leaflet
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import qualified Korrvigs.Web.Vis.Network as Network
import Opaleye hiding (groupBy, null)
import Text.Julius (rawJS)
import Yesod

-- An entry page is constitued of the following parts
-- The entry title (if any) + the entry name
-- A link to download the entry
-- The metadata in a foldable blocks, with edition
-- The backlinks,parents and subs
-- The entry content

-- Takes the ID of the div containing the content
titleWidget :: Entry -> Text -> Handler Widget
titleWidget entry contentId = do
  title :: Maybe Text <- fmap join $ rSelectOne $ do
    mtdt <- selectTable entriesMetadataTable
    where_ $ (mtdt ^. sqlEntry) .== sqlId (entry ^. name)
    where_ $ mtdt ^. sqlKey .== sqlStrictText "title"
    pure $ sqlJsonToText $ toNullable $ mtdt ^. sqlValue
  medit <- editWidget
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
      $maybe edit <- medit
        ^{edit}
  |]
  where
    editWidget :: Handler (Maybe Widget)
    editWidget = case entry ^. kindData of
      LinkD _ -> pure Nothing
      FileD _ -> pure Nothing
      EventD _ -> pure Nothing
      NoteD _ -> Just <$> Note.editButton (entry ^. name) 0 contentId (SubLoc [])

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

newFormWidget :: [Text] -> Entry -> Handler Widget
newFormWidget errMsgs entry = do
  nw <- Home.newForms (EntryR $ WId $ entry ^. name) "Attach" errMsgs
  pure
    [whamlet|
    <details>
      <summary>Attach entry
      ^{nw}
  |]

entryWidget :: [Text] -> Entry -> Handler Widget
entryWidget errMsgs entry = do
  contentId <- newIdent
  title <- titleWidget entry contentId
  dt <- dateWidget entry
  geom <- geometryWidget entry
  mtdt <- Mtdt.widget entry
  refs <- refsWidget entry
  content <- contentWidget entry
  nw <- newFormWidget errMsgs entry
  pure $ do
    Rcs.entryStyle
    Rcs.formsStyle
    title
    dt
    nw
    geom
    mtdt
    refs
    [whamlet|
      <div ##{contentId}>
        ^{content}
    |]

getEntryR :: WebId -> Handler Html
getEntryR (WId i) =
  load i >>= \case
    Just entry -> entryWidget [] entry >>= defaultLayout
    Nothing -> notFound

postEntryR :: WebId -> Handler Html
postEntryR (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> do
      (err, w) <-
        Home.runNewForms (Just i) >>= \case
          Nothing -> pure ([], mempty)
          Just (Left err) -> pure (err, mempty)
          Just (Right ni) -> pure ([], copyToClipboard ni)
      entryW <- entryWidget err entry
      defaultLayout $ w >> entryW
  where
    -- TODO find how to copy to clipboard
    copyToClipboard :: Id -> Widget
    copyToClipboard (MkId ni) = do
      ident <- rawJS <$> newIdent
      toWidget
        [julius|
        function #{ident}() {
          alert("Created " + #{ni})
        }
        #{ident}()
      |]
