module Korrvigs.Web.Entry (getEntryR, postEntryR) where

import Control.Lens hiding (children)
import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime
import Korrvigs.Actions
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Collections
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Note.Loc (SubLoc (SubLoc))
import Korrvigs.Utils.Base16
import Korrvigs.Utils.Opaleye (connectedComponentGraph)
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Entry.Calendar as Cal
import qualified Korrvigs.Web.Entry.Event as Event
import qualified Korrvigs.Web.Entry.File as File
import qualified Korrvigs.Web.Entry.Link as Link
import qualified Korrvigs.Web.Entry.Metadata as Mtdt
import qualified Korrvigs.Web.Entry.Note as Note
import qualified Korrvigs.Web.Home as Home
import Korrvigs.Web.Leaflet
import qualified Korrvigs.Web.PhotoSwipe as PhotoSwipe
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import qualified Korrvigs.Web.Vis.Network as Network
import qualified Korrvigs.Web.Widgets as Wdgs
import Opaleye hiding (groupBy, not, null)
import qualified Opaleye as O
import Text.Blaze (toMarkup)
import Text.Julius (rawJS)
import Yesod hiding (Field)

-- Takes the ID of the div containing the content
titleWidget :: Entry -> Text -> Handler Widget
titleWidget entry contentId = do
  title <- rSelectTextMtdt Title $ sqlId $ entry ^. name
  favourite <- rSelectMtdt Favourite $ sqlId $ entry ^. name
  taskW <- Wdgs.taskWidget (entry ^. name) (SubLoc []) =<< loadTask (entry ^. name)
  medit <- editWidget
  pure $ do
    case title of
      Just t -> setTitle $ toMarkup t
      Nothing -> setTitle $ toMarkup $ "@" <> unId (entry ^. name)
    [whamlet|
    ^{htmlKind $ entry ^. kind}
    <span .download-button>
      <a href=@{EntryDownloadR $ WId $ entry ^. name}>
        ⬇
    <h1>
      $maybe _ <- favourite
        ★
      ^{taskW}
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
      CalendarD _ -> pure Nothing
      NoteD _ -> Just <$> Note.editButton (entry ^. name) 0 Nothing contentId (SubLoc [])

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
  let entries = map head $ groupBy (\r1 r2 -> cmp r1 r2 == EQ) $ sortBy cmp rows
  nodes <- mapM mkNode entries
  edgeStyle <- Network.defEdgeStyle
  base <- getBase
  let edges = mkEdge edgeStyle base <$> graph
  network <- if null entries then pure mempty else Network.network "network" nodes edges
  detId <- newIdent
  parInputId <- newIdent
  parConfirmId <- newIdent
  parRmId <- newIdent
  pure $ do
    [whamlet|
      <details .common-details ##{detId}>
        <summary>Network
        ^{network}
        <table>
          <tr>
            <td .mtdt-key>
              <input ##{parInputId} .mtdt-input type=text>
            <td .mtdt-button-case>
              <button ##{parConfirmId} .mtdt-button .mtdt-confirm-button>✎
            <td .mtdt-button-case>
              <button ##{parRmId} .mtdt-button .mtdt-rm-button>❌
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
    toWidget
      [julius|
      document.getElementById(#{parConfirmId}).addEventListener("click", function() {
        const parInput = document.getElementById(#{parInputId})
        fetch("@{EntryParentsR $ WId i}", {
          method: "POST",
          body: JSON.stringify({ add: [parInput.value], remove: [] }),
          headers: {
            "Content-Type": "application/json; charset=utf-8"
          }
        })
        parInput.value = ""
      })
      document.getElementById(#{parRmId}).addEventListener("click", function() {
        const parInput = document.getElementById(#{parInputId})
        fetch("@{EntryParentsR $ WId i}", {
          method: "POST",
          body: JSON.stringify({ add: [], remove: [parInput.value] }),
          headers: {
            "Content-Type": "application/json; charset=utf-8"
          }
        })
        parInput.value = ""
      })
    |]
  where
    cmp :: EntryRow -> EntryRow -> Ordering
    cmp row1 row2 = compare (row1 ^. sqlEntryName) (row2 ^. sqlEntryName)
    relEntries :: Table a RelRowSQL -> Bool -> Select (Field SqlText, Field SqlText, Field SqlBool)
    relEntries tbl isSub = do
      subs <- selectTable tbl
      pure (subs ^. source, subs ^. target, sqlBool isSub)
    i = entry ^. name
    notesCC :: Select (EntryRowSQL, EntryRowSQL, Field SqlBool)
    notesCC = do
      cc <-
        connectedComponentGraph
          (unionAll (relEntries entriesSubTable True) (relEntries entriesRefTable False))
          (view _1)
          (view _2)
          ( \(eid1, eid2, isSub) -> do
              e1 <- selectTable entriesTable
              where_ $ e1 ^. sqlEntryName .== eid1
              e2 <- selectTable entriesTable
              where_ $ e2 ^. sqlEntryName .== eid2
              pure $
                O.not isSub
                  .|| eid1
                  .== sqlId i
                  .|| O.not (isPair e1 e2 [(Event, Calendar), (File, Event), (File, Note)])
          )
          (pure (sqlId i, sqlId i, sqlBool True))
      e1 <- selectTable entriesTable
      where_ $ (e1 ^. sqlEntryName) .== (cc ^. _1)
      e2 <- selectTable entriesTable
      where_ $ (e2 ^. sqlEntryName) .== (cc ^. _2)
      pure (e1, e2, cc ^. _3)
    isPair :: EntryRowSQL -> EntryRowSQL -> [(Kind, Kind)] -> Field SqlBool
    isPair e1 e2 = foldr (\kds b -> b .|| checkPair e1 e2 kds) (sqlBool False)
    checkPair :: EntryRowSQL -> EntryRowSQL -> (Kind, Kind) -> Field SqlBool
    checkPair e1 e2 (kd1, kd2) =
      (e1 ^. sqlEntryKind)
        .== sqlKind kd1
        .&& (e2 ^. sqlEntryKind)
        .== sqlKind kd2
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
    subStyle edgeStyle base = edgeStyle & Network.edgeColor .~ base edgeSubColor
    refStyle edgeStyle base = edgeStyle & Network.edgeColor .~ base edgeRefColor
    mkEdge :: Network.EdgeStyle -> (Base16Index -> Text) -> (EntryRow, EntryRow, Bool) -> (Text, Text, Network.EdgeStyle)
    mkEdge edgeStyle base (r1, r2, isSub) =
      let mkStyle = if isSub then subStyle else refStyle
       in let style = mkStyle edgeStyle base
           in (unId $ r1 ^. sqlEntryName, unId $ r2 ^. sqlEntryName, style)

galleryWidget :: Entry -> Handler Widget
galleryWidget entry =
  rSelectMtdt Gallery (sqlId $ entry ^. name) >>= \case
    Nothing -> pure mempty
    Just gallery -> do
      let select = if gallery == "recursive" then selectRecSourcesFor else selectSourcesFor
      childs <- rSelect $ orderBy (ascNullsFirst (^. sqlEntryDate)) $ do
        sub <- select entriesSubTable $ entry ^. name
        subEntry <- selectTable entriesTable
        where_ $ sub .== subEntry ^. sqlEntryName
        where_ $ subEntry ^. sqlEntryKind .== sqlKind File
        pure subEntry
      if null childs
        then pure mempty
        else do
          entries <- mapM mkEntry childs
          photoswipe <- PhotoSwipe.photoswipe $ catMaybes entries
          pure
            [whamlet|
          <details .common-details>
            <summary>Gallery
            <div #common-gallery>
              ^{photoswipe}
        |]
  where
    mkEntry :: EntryRow -> Handler (Maybe PhotoSwipe.PhotoswipeEntry)
    mkEntry e =
      PhotoSwipe.miniatureEntry
        (e ^? sqlEntryDate . _Just . to zonedTimeToLocalTime . to localDay)
        (e ^. sqlEntryName)

contentWidget :: Entry -> Handler Widget
contentWidget entry = case entry ^. kindData of
  LinkD link -> Link.content link
  NoteD note -> Note.content note
  FileD file -> File.content file
  EventD event -> Event.content event
  CalendarD cal -> Cal.content cal

colsWidget :: Entry -> Handler Widget
colsWidget entry = do
  let i = entry ^. name
  mfavs <- fromMaybe [] <$> rSelectMtdt Favourite (sqlId i)
  mmisc <- fromMaybe [] <$> rSelectMtdt MiscCollection (sqlId i)
  mgals <- fromMaybe [] <$> rSelectMtdt GalleryCollection (sqlId i)
  let cols =
        [ ("Favourites", ColFavouriteR, mfavs),
          ("Miscellaneous", ColMiscR, mmisc),
          ("Gallery", ColGalR, mgals)
        ] ::
          [(Text, [Text] -> Route WebData, [[Text]])]
  pure $
    unless
      (null mfavs && null mmisc && null mgals)
      [whamlet|
  <details .common-details>
    <summary>Collections
    <ul>
      $forall (title,lnk,subcols) <- cols
        $if (not $ null subcols)
          <li>
            <a href=@{lnk []}>#{title}
            <ul>
              $forall col <- subcols
                <li>
                  <a href=@{lnk col}>
                    #{T.intercalate " > " col}
  |]

newFormWidget :: [Text] -> Entry -> Handler Widget
newFormWidget errMsgs entry = do
  nw <- Home.newForms (EntryR $ WId $ entry ^. name) "Attach" errMsgs
  pure
    [whamlet|
    <details .common-details>
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
  cols <- colsWidget entry
  refs <- refsWidget entry
  gallery <- galleryWidget entry
  content <- contentWidget entry
  nw <- newFormWidget errMsgs entry
  pure $ do
    Rcs.entryStyle
    Rcs.formsStyle
    PhotoSwipe.photoswipeHeader
    title
    dt
    nw
    geom
    mtdt
    cols
    refs
    gallery
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
