{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Web.Search.Results where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson.Lens
import Data.Default
import Data.Foldable
import Data.List (intersperse)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata.Contact (reifyContactData)
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Note (Collection (..))
import Korrvigs.Utils
import Korrvigs.Utils.Base16
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Time
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.JS.FullCalendar as Cal
import qualified Korrvigs.Web.JS.Fuse as Fuse
import Korrvigs.Web.JS.Leaflet
import qualified Korrvigs.Web.JS.PhotoSwipe as PhotoSwipe
import Korrvigs.Web.Public.Crypto
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import qualified Korrvigs.Web.Vis.Network as Network
import qualified Korrvigs.Web.Vis.Timeline as Timeline
import qualified Korrvigs.Web.Widgets as Wdgs
import Opaleye hiding (Field, not)
import qualified Opaleye as O
import qualified Text.Blaze.Html5 as H
import Yesod

displayEntry :: ColEntry -> Handler Html
displayEntry entry = do
  public <- isPublic
  (kd, isDummy) <- case entry ^. sqlEntryKind of
    Just k -> (,False) <$> htmlKind' k
    Nothing -> pure (mempty, True)
  [hamlet|
    #{kd}
    $if public || isDummy
      ^{plain}
    $else
      <a href=@{EntryR $ WId $ entry ^. sqlEntryName}>
        ^{plain}
  |]
    <$> getUrlRenderParams
  where
    title = entry ^. sqlEntryTitle
    plain = case title of
      Just t -> [hamlet|#{t}|]
      Nothing -> [hamlet|@#{unId $ entry ^. sqlEntryName}|]

displayResults :: Collection -> Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayResults ColList = displayList
displayResults ColMap = displayMap
displayResults ColGallery = displayGallery
displayResults ColTimeline = displayTimeline
displayResults ColNetwork = displayGraph
displayResults ColFuzzy = displayFuzzy
displayResults ColCalendar = displayCalendar
displayResults ColKanban = displayUnsupported ColKanban
displayResults ColTaskList = displayTaskList
displayResults ColLibrary = displayLibrary
displayResults ColPlayList = displayPlayList
displayResults ColContacts = displayContacts

displayUnsupported :: Collection -> Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayUnsupported col _ _ =
  pure [whamlet|<p>#{show col} is not supported yet|]

displayList :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayList _ entries' = do
  let entries = fst <$> entries'
  entriesH <- mapM displayEntry entries
  pure
    [whamlet|
      <ul>
        $forall entry <- entriesH
          <li>
            #{entry}
    |]

displayMap :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayMap _ entries = do
  items <- mapM mkItem entries
  mapId <- newIdent
  pure $ leafletWidget mapId [] $ catMaybes items
  where
    mkItem :: (ColEntry, OptionalSQLData) -> Handler (Maybe MapItem)
    mkItem (entry, _) = case entry ^. sqlEntryGeo of
      Just geom -> do
        html <- displayEntry entry
        pure $
          Just $
            MapItem
              { _mitGeo = geom,
                _mitContent = Just html,
                _mitVar = Nothing,
                _mitColor = Nothing
              }
      Nothing -> pure Nothing

displayGraph :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayGraph _ entries = do
  nodes <- mapM mkNode entries
  let selectPairs tbl = do
        rel <- selectTable tbl
        where_ $ sqlElem (rel ^. source) candidates
        where_ $ sqlElem (rel ^. target) candidates
        srcE <- selectTable entriesTable
        where_ $ srcE ^. sqlEntryId .== (rel ^. source)
        dstE <- selectTable entriesTable
        where_ $ dstE ^. sqlEntryId .== (rel ^. target)
        pure (srcE ^. sqlEntryName, dstE ^. sqlEntryName)
  subs <- rSelect $ selectPairs entriesSubTable
  refs <- rSelect $ selectPairs entriesRefTable
  base <- getBase
  edgeStyle <- Network.defEdgeStyle
  let subStyle = edgeStyle & Network.edgeColor .~ base edgeSubColor
  let refStyle = edgeStyle & Network.edgeColor .~ base edgeRefColor
  let edges = (mkEdge subStyle <$> subs) ++ (mkEdge refStyle <$> refs)
  networkId <- newIdent
  Network.network networkId nodes edges
  where
    isCandidate entry = entry ^. _1 . sqlEntryKind . to isJust
    candidates :: O.Field (SqlArray SqlInt4)
    candidates =
      sqlArray sqlInt4 $ view (_1 . sqlEntryId) <$> filter isCandidate entries
    mkNode :: (ColEntry, OptionalSQLData) -> Handler (Text, Text, Network.NodeStyle)
    mkNode (entry, _) = do
      let isDummy = isNothing $ entry ^. sqlEntryKind
      public <- isPublic
      style <- Network.defNodeStyle
      render <- getUrlRender
      let mrender url = if public || isDummy then Nothing else Just (render url)
      base <- getBase
      let caption = case entry ^. sqlEntryTitle of
            Just t -> t
            Nothing -> "@" <> unId (entry ^. sqlEntryName)
      let color = base $ maybe Base0F colorKind $ entry ^. sqlEntryKind
      pure
        ( unId $ entry ^. sqlEntryName,
          caption,
          style
            & Network.nodeBorder .~ color
            & Network.nodeBackground .~ color
            & Network.nodeSelected .~ color
            & Network.nodeLink .~ mrender (EntryR $ WId $ entry ^. sqlEntryName)
        )
    mkEdge :: Network.EdgeStyle -> (Id, Id) -> (Text, Text, Network.EdgeStyle)
    mkEdge style (src, dst) = (unId src, unId dst, style)

displayTimeline :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayTimeline _ entries = do
  items <- mapM mkItem entries
  timelineId <- newIdent
  Timeline.timeline timelineId $ catMaybes items
  where
    mkItem :: (ColEntry, OptionalSQLData) -> Handler (Maybe Timeline.Item)
    mkItem (entry, _) = do
      let isDummy = isNothing $ entry ^. sqlEntryKind
      public <- isPublic
      render <- getUrlRender
      let mrender url = if public || isDummy then Nothing else Just (render url)
      let caption = case entry ^. sqlEntryTitle of
            Just t -> t
            Nothing | not isDummy -> "@" <> unId (entry ^. sqlEntryName)
            _ -> "<dummy>"
      pure $ case entry ^. sqlEntryDate of
        Nothing -> Nothing
        Just start ->
          let end = case entry ^. sqlEntryDuration of
                Nothing -> Nothing
                Just dur -> Just $ addCalendar dur start
           in Just $
                Timeline.Item
                  { Timeline._itemText = caption,
                    Timeline._itemStart = start,
                    Timeline._itemEnd = end,
                    Timeline._itemGroup = maybe "dummy" displayKind $ entry ^. sqlEntryKind,
                    Timeline._itemTarget = mrender $ EntryR $ WId $ entry ^. sqlEntryName
                  }

displayGallery :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayGallery isCol entries = do
  public <- isPublic
  items <- forM entries $ \e -> runMaybeT $ do
    entry <-
      lift
        ( PhotoSwipe.miniatureEntry
            (e ^. _2 . optMime)
            (e ^? _1 . sqlEntryDate . _Just . to zonedTimeToLocalTime . to localDay)
            (e ^. _1 . sqlEntryName)
        )
        >>= hoistMaybe
    pure $ if public then entry & PhotoSwipe.swpRedirect .~ Nothing else entry
  gallery <- PhotoSwipe.photoswipe (def & PhotoSwipe.swpGroup .~ not isCol) $ catMaybes items
  pure $ do
    PhotoSwipe.photoswipeHeader
    gallery

displayLibrary :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayLibrary _ entries = do
  public <- isPublic
  items <- forM entries $ \e -> runMaybeT $ do
    let isDummy = isNothing $ e ^. _1 . sqlEntryKind
    coverId <- hoistMaybe $ MkId <$> e ^. _2 . optCover
    entry <- hoistLift $ PhotoSwipe.miniatureEntry (e ^. _2 . optMime) Nothing coverId
    let title :: [(Text, Text)] = [("title", t) | t <- toList $ e ^. _1 . sqlEntryTitle]
    caption <- lift $ mkTaskItem public (e ^. _1) (e ^. _2 & optAggregCount .~ Nothing)
    pure $
      entry
        & PhotoSwipe.swpCaption .~ [whamlet|<p *{title}>^{caption}|]
        & PhotoSwipe.swpRedirect .~ (if public || isDummy then Nothing else Just (EntryR $ WId $ e ^. _1 . sqlEntryName))
        & PhotoSwipe.swpLabel .~ (e ^? _2 . optAggregCount . _Just . _Integer . to renderCount . _Just)
  library <- PhotoSwipe.photoswipe (def & PhotoSwipe.swpLibrary .~ True) $ catMaybes items
  cssR <- mkCss
  pure $ do
    Rcs.entryStyle cssR
    PhotoSwipe.photoswipeHeader
    Rcs.checkboxCode StaticR
    library
  where
    renderCount :: Integer -> Maybe Text
    renderCount 0 = Nothing
    renderCount i = Just . T.pack . show $ i

displayFuzzy :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayFuzzy _ entries = do
  items <- forM entries $ mkFuseItem . view _1
  fuse <- Fuse.widget $ catMaybes items
  pure $ do
    Fuse.header
    fuse
  where
    mkFuseItem e = case e ^. sqlEntryKind of
      Just _ -> Just <$> Fuse.itemFromEntry (e ^. sqlEntryName, e ^. sqlEntryTitle)
      Nothing -> case e ^. sqlEntryTitle of
        Just t ->
          pure $
            Just $
              Fuse.FuseItem
                { Fuse._itDisplay = H.toMarkup t,
                  Fuse._itText = t,
                  Fuse._itSubText = ""
                }
        Nothing -> pure Nothing

displayCalendar :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayCalendar _ entries = do
  events <- forM entries $ Cal.colEntryToEvent . fst
  cal <- Cal.widget $ catMaybes events
  pure $ do
    Cal.header
    cal

displayTaskList :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayTaskList _ entries = do
  public <- isPublic
  items <- mapM (uncurry $ mkTaskItem public) entries
  cssR <- mkCss
  pure $ do
    Rcs.entryStyle cssR
    Rcs.checkboxCode StaticR
    [whamlet|
      <ul>
        $forall item <- items
          <li>
            ^{item}
    |]

mkTaskItem :: Bool -> ColEntry -> OptionalSQLData -> Handler Widget
mkTaskItem public entry dat = do
  let isDummy = isNothing (entry ^. sqlEntryKind)
  let i = entry ^. sqlEntryName
  let mAgCount :: Maybe Int = dat ^. optAggregCount >>= fromJSONM
  let cbDWIM = if isDummy then const Wdgs.checkBoxUninteractiveDWIM else Wdgs.checkBoxDWIM
  cb <- cbDWIM i $ dat ^. optTask
  pure
    [whamlet|
    ^{cb}
    #{T.pack " "}
    $maybe agCount <- mAgCount
      <span .aggregate-count>
        #{show agCount}
    $if public || isDummy
      ^{plainTitle i (view sqlEntryTitle entry)}
    $else
      <a href=@{EntryR $ WId i}>
        ^{plainTitle i (view sqlEntryTitle entry)}
  |]
  where
    plainTitle :: Id -> Maybe Text -> Widget
    plainTitle i title =
      [whamlet|
      $maybe t <- title
        #{t}
      $nothing
        @#{unId i}
    |]

displayPlayList :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayPlayList _ entries = do
  public <- isPublic
  items <- mapM (mkItem public) entries
  cssR <- mkCss
  pure $ do
    Rcs.entryStyle cssR
    Rcs.checkboxCode StaticR
    [whamlet|
      <div .playlist>
        <ul>
          $forall item <- items
            ^{item}
    |]
  where
    mkItem public (entry, dat) = do
      let i = entry ^. sqlEntryName
      route <- mkPublic $ EntryDownloadR $ WId i
      title <- mkTaskItem public entry dat
      pure
        [whamlet|
        <li>
          ^{title}
          <audio controls autoplay=false loading=lazy src=@{route}>
      |]

displayContacts :: Bool -> [(ColEntry, OptionalSQLData)] -> Handler Widget
displayContacts _ entries = do
  widgets <- forM entries $ \(entry, dat) -> case dat ^. optContact of
    Nothing -> pure mempty
    Just contact ->
      let nm = if isNothing (entry ^. sqlEntryKind) then Nothing else Just (entry ^. sqlEntryName)
       in Wdgs.mkContactWidget nm (reifyContactData contact)
  pure $ do
    Rcs.contactStyle CssR
    sequence_ $ intersperse sep widgets
  where
    sep :: Widget
    sep = toWidget $ H.hr
