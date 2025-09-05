module Korrvigs.Web.Entry (getEntryR, isPrivate) where

import Control.Lens hiding (children)
import Control.Monad
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime
import Korrvigs.Compute.Action
import Korrvigs.Entry
import Korrvigs.File.SQL
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Note.Loc (SubLoc (SubLoc))
import Korrvigs.Utils.Base16
import Korrvigs.Utils.Opaleye (connectedComponentGraph)
import Korrvigs.Web.Actions
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Calendar as Cal
import qualified Korrvigs.Web.Entry.Event as Event
import qualified Korrvigs.Web.Entry.File as File
import qualified Korrvigs.Web.Entry.Link as Link
import qualified Korrvigs.Web.Entry.Metadata as Mtdt
import qualified Korrvigs.Web.Entry.Note as Note
import Korrvigs.Web.Leaflet
import qualified Korrvigs.Web.PhotoSwipe as PhotoSwipe
import qualified Korrvigs.Web.Public.Crypto as Public
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import qualified Korrvigs.Web.Vis.Network as Network
import qualified Korrvigs.Web.Widgets as Wdgs
import Opaleye hiding (groupBy, not, null)
import qualified Opaleye as O
import Text.Blaze (toMarkup)
import Yesod hiding (Field)

-- Takes the ID of the div containing the content
titleWidget :: Entry -> Text -> Handler Widget
titleWidget entry contentId = do
  public <- isPublic
  let title = entry ^. entryTitle
  taskW <- Wdgs.taskWidget (entry ^. entryName) (SubLoc []) =<< loadTask (entry ^. entryName) (entry ^. entryId) title
  medit <- if public then pure mempty else editWidget
  downloadUrl <- Public.mkPublic $ EntryDownloadR $ WId $ entry ^. entryName
  pure $ do
    case title of
      Just t -> setTitle $ toMarkup t
      Nothing -> setTitle $ toMarkup $ "@" <> unId (entry ^. entryName)
    [whamlet|
    ^{htmlKind $ entry ^. kind}
    <span .download-button>
      <a href=@{downloadUrl}>
        ⬇
    <h1>
      ^{taskW}
      $maybe t <- title
        #{t}
      <span .entry-entryName>
        (#{unId $ entry ^. entryName})
      $maybe edit <- medit
        ^{edit}
  |]
  where
    editWidget :: Handler (Maybe Widget)
    editWidget = case entry ^. entryKindData of
      LinkD _ -> pure Nothing
      FileD _ -> pure Nothing
      EventD _ -> pure Nothing
      CalendarD _ -> pure Nothing
      NoteD _ -> Just <$> Note.editButton (entry ^. entryName) 0 Nothing contentId (SubLoc [])

-- TODO make link to day viewer
dateWidget :: Entry -> Handler Widget
dateWidget entry = case entry ^. entryDate of
  Just time ->
    pure
      [whamlet|
      <a href="">
        #{iso8601Show time}
    |]
  Nothing -> pure mempty

geometryWidget :: Entry -> Handler Widget
geometryWidget entry = case entry ^. entryGeo of
  Nothing -> pure mempty
  Just geometry -> do
    detClass <- newIdent
    pure $ do
      [whamlet|
        <details .common-details .#{detClass}>
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
  graph <- filter (\(e1, e2, _) -> e1 ^. sqlEntryName /= e2 ^. sqlEntryName) <$> rSelect notesCC
  let rows = (view _1 <$> graph) ++ (view _2 <$> graph)
  let entries = map NE.head $ NE.groupBy (\r1 r2 -> cmp r1 r2 == EQ) $ sortBy cmp rows
  nodes <- mapM mkNode entries
  edgeStyle <- Network.defEdgeStyle
  base <- getBase
  let edges = mkEdge edgeStyle base <$> graph
  network <- if null entries then pure mempty else Network.network "network" nodes edges
  detId <- newIdent
  pure $ unless (null entries) $ do
    [whamlet|
      <details .common-details ##{detId}>
        <summary>Network
        ^{network}
    |]
    unless (null entries) $
      toWidget
        [julius|
      document.getElementById(#{detId}).addEventListener("toggle", function(e) {
        if (e.newState == "open") {
          network.fit()
        }
      })
      |]
  where
    cmp :: EntryRowR -> EntryRowR -> Ordering
    cmp row1 row2 = compare (row1 ^. sqlEntryName) (row2 ^. sqlEntryName)
    relEntries :: Table a RelRowSQL -> Bool -> Select (Field SqlInt4, Field SqlInt4, Field SqlBool)
    relEntries tbl isSub = do
      subs <- selectTable tbl
      pure (subs ^. source, subs ^. target, sqlBool isSub)
    i = entry ^. entryName
    notesCC :: Select (EntryRowSQLR, EntryRowSQLR, Field SqlBool)
    notesCC = do
      cc <-
        connectedComponentGraph
          (unionAll (relEntries entriesSubTable True) (relEntries entriesRefTable False))
          (view _1)
          (view _2)
          ( \(eid1, eid2, isSub) orient -> do
              e1 <- selectTable entriesTable
              where_ $ e1 ^. sqlEntryId .== eid1
              let nm1 = e1 ^. sqlEntryName
              where_ $ (e1 ^. sqlEntryKind) `sqlElem` toFields [Note, Link, Calendar, Event] .|| nm1 .== sqlId i
              e2 <- selectTable entriesTable
              where_ $ e2 ^. sqlEntryId .== eid2
              where_ $ (e2 ^. sqlEntryKind) `sqlElem` toFields [Note, Link, Calendar, Event]
              hub1 <- O.not . isNull <$> selectTextMtdt HubMtdt eid1
              hub2 <- O.not . isNull <$> selectTextMtdt HubMtdt eid2
              where_ $
                (O.not orient .&& (O.not hub1 .|| hub2))
                  .|| (orient .&& (O.not hub2 .|| hub1))
              where_ $
                O.not isSub
                  .|| nm1
                  .== sqlId i
                  .|| O.not (isPair e1 e2 [(Event, Calendar)])
          )
          (pure (sqlInt4 $ entry ^. entryId, sqlInt4 $ entry ^. entryId, sqlBool True))
      e1 <- selectTable entriesTable
      where_ $ (e1 ^. sqlEntryId) .== (cc ^. _1)
      e2 <- selectTable entriesTable
      where_ $ (e2 ^. sqlEntryId) .== (cc ^. _2)
      pure (e1, e2, cc ^. _3)
    isPair :: EntryRowSQLR -> EntryRowSQLR -> [(Kind, Kind)] -> Field SqlBool
    isPair e1 e2 = foldr (\kds b -> b .|| checkPair e1 e2 kds) (sqlBool False)
    checkPair :: EntryRowSQLR -> EntryRowSQLR -> (Kind, Kind) -> Field SqlBool
    checkPair e1 e2 (kd1, kd2) =
      (e1 ^. sqlEntryKind)
        .== sqlKind kd1
        .&& (e2 ^. sqlEntryKind)
        .== sqlKind kd2
    mkNode :: EntryRowR -> Handler (Text, Text, Network.NodeStyle)
    mkNode e = do
      style <- Network.defNodeStyle
      render <- getUrlRender
      base <- getBase
      let color =
            if e ^. sqlEntryName == entry ^. entryName
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
    subStyle edgeStyle base = edgeStyle & Network.edgeColor .~ base edgeSubColor
    refStyle edgeStyle base = edgeStyle & Network.edgeColor .~ base edgeRefColor
    mkEdge :: Network.EdgeStyle -> (Base16Index -> Text) -> (EntryRowR, EntryRowR, Bool) -> (Text, Text, Network.EdgeStyle)
    mkEdge edgeStyle base (r1, r2, isSub) =
      let mkStyle = if isSub then subStyle else refStyle
       in let style = mkStyle edgeStyle base
           in (unId $ r1 ^. sqlEntryName, unId $ r2 ^. sqlEntryName, style)

subWidget :: Entry -> Handler Widget
subWidget entry = do
  let i = entry ^. entryId
  subs :: [(Id, Maybe Text)] <- rSelect $ orderBy ord $ do
    e <- selectTable entriesTable
    rel <- selectTable entriesSubTable
    where_ $ (e ^. sqlEntryId) .== rel ^. source
    where_ $ rel ^. target .== sqlInt4 i
    pure (e ^. sqlEntryName, e ^. sqlEntryTitle)
  pure $
    if null subs
      then mempty
      else
        [whamlet|
    <details .common-details>
      <summary>Attached
      <ul>
        $forall (sub,title) <- subs
          <li>
            <a href=@{EntryR $ WId sub}>
              $maybe t <- title
                #{t}
              $nothing
                #{"@" <> unId sub}
  |]
  where
    ord = ascNullsLast snd <> asc fst

galleryWidget :: Entry -> Handler Widget
galleryWidget entry =
  rSelectMtdt Gallery (sqlId $ entry ^. entryName) >>= \case
    Nothing -> pure mempty
    Just gallery -> do
      let select = if gallery == "recursive" then selectRecSourcesFor else selectSourcesFor
      childs <- rSelect $ orderBy (ascNullsFirst (^. _1 . sqlEntryDate)) $ do
        sub <- select entriesSubTable $ sqlInt4 $ entry ^. entryId
        subEntry <- selectTable entriesTable
        where_ $ sub .== subEntry ^. sqlEntryId
        where_ $ subEntry ^. sqlEntryKind .== sqlKind File
        void $ selComp sub "miniature"
        sz <- selComp sub "size"
        mime <- optional $ do
          file <- selectTable filesTable
          where_ $ file ^. sqlFileId .== sub
          pure $ file ^. sqlFileMime
        pure (subEntry, mime, sz ^. sqlCompAction)
      if null childs
        then pure mempty
        else do
          entries <- mapM mkEntry childs
          photoswipe <- PhotoSwipe.photoswipe False $ catMaybes entries
          pure
            [whamlet|
          <details .common-details>
            <summary>Gallery
            <div #common-gallery>
              ^{photoswipe}
        |]
  where
    mkEntry :: (EntryRowR, Maybe Text, Action) -> Handler (Maybe PhotoSwipe.PhotoswipeEntry)
    mkEntry (e, mime, sizeA) =
      PhotoSwipe.miniatureEntry
        mime
        (e ^? sqlEntryDate . _Just . to zonedTimeToLocalTime . to localDay)
        (e ^. sqlEntryName)
        sizeA

contentWidget :: Entry -> Handler Widget
contentWidget entry = case entry ^. entryKindData of
  LinkD link -> Link.content link
  NoteD note -> Note.content note
  FileD file -> File.content file
  EventD event -> Event.content event
  CalendarD cal -> Cal.content cal

actWidget :: Entry -> Handler Widget
actWidget entry = do
  actions <- actionsWidget $ TargetEntry entry
  pure
    [whamlet|
    <details .common-details>
      <summary>Actions
      ^{actions}
  |]

isPrivate :: Entry -> Handler Bool
isPrivate entry =
  rSelectTextMtdt Private (sqlId $ entry ^. entryName) >>= \case
    Nothing -> pure False
    _ -> pure True

entryWidget :: Entry -> Handler Widget
entryWidget entry = do
  public <- isPublic
  private <- isPrivate entry
  when (public && private) $ permissionDenied "Tried to access a private entry"
  contentId <- newIdent
  title <- titleWidget entry contentId
  dt <- dateWidget entry
  geom <- geometryWidget entry
  mtdt <- Mtdt.widget entry
  refs <- refsWidget entry
  subs <- subWidget entry
  gallery <- galleryWidget entry
  content <- contentWidget entry
  actions <- actWidget entry
  pure $ do
    Rcs.entryStyle
    Rcs.formsStyle
    Rcs.checkboxCode
    PhotoSwipe.photoswipeHeader
    title
    unless public $ do
      dt
      actions
      geom
      mtdt
      refs
      subs
      gallery
    [whamlet|
      <div ##{contentId}>
        ^{content}
    |]

getEntryR :: WebId -> Handler Html
getEntryR (WId i) =
  load i >>= \case
    Just entry -> entryWidget entry >>= defaultLayout
    Nothing -> notFound
