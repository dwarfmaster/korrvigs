{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Web.Search.Results where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Note (Collection (..))
import Korrvigs.Utils
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Time
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.FullCalendar as Cal
import qualified Korrvigs.Web.Fuse as Fuse
import Korrvigs.Web.Leaflet
import qualified Korrvigs.Web.PhotoSwipe as PhotoSwipe
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import qualified Korrvigs.Web.Vis.Network as Network
import qualified Korrvigs.Web.Vis.Timeline as Timeline
import qualified Korrvigs.Web.Widgets as Wdgs
import Opaleye hiding (Field, not)
import qualified Opaleye as O
import Yesod

displayEntry :: EntryRowR -> Handler Html
displayEntry entry = do
  public <- isPublic
  kd <- htmlKind' $ entry ^. sqlEntryKind
  [hamlet|
    #{kd}
    $if public
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

displayResults :: Collection -> Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
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

displayUnsupported :: Collection -> Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
displayUnsupported col _ _ =
  pure [whamlet|<p>#{show col} is not supported yet|]

displayList :: Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
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

displayMap :: Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
displayMap _ entries = do
  items <- mapM mkItem entries
  pure $ leafletWidget "resultmap" $ catMaybes items
  where
    mkItem :: (EntryRowR, OptionalSQLData) -> Handler (Maybe MapItem)
    mkItem (entry, _) = case entry ^. sqlEntryGeo of
      Just geom -> do
        html <- displayEntry entry
        pure $
          Just $
            MapItem
              { _mitGeo = geom,
                _mitContent = Just html,
                _mitVar = Nothing
              }
      Nothing -> pure Nothing

displayGraph :: Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
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
  Network.network "network" nodes edges
  where
    candidates :: O.Field (SqlArray SqlInt4)
    candidates = sqlArray sqlInt4 $ view (_1 . sqlEntryId) <$> entries
    mkNode :: (EntryRowR, OptionalSQLData) -> Handler (Text, Text, Network.NodeStyle)
    mkNode (entry, _) = do
      public <- isPublic
      style <- Network.defNodeStyle
      render <- getUrlRender
      let mrender url = if public then Nothing else Just (render url)
      base <- getBase
      let caption = case entry ^. sqlEntryTitle of
            Just t -> t
            Nothing -> "@" <> unId (entry ^. sqlEntryName)
      let color = base $ colorKind $ entry ^. sqlEntryKind
      pure
        ( unId $ entry ^. sqlEntryName,
          caption,
          style
            & Network.nodeBorder .~ color
            & Network.nodeSelected .~ color
            & Network.nodeLink .~ mrender (EntryR $ WId $ entry ^. sqlEntryName)
        )
    mkEdge :: Network.EdgeStyle -> (Id, Id) -> (Text, Text, Network.EdgeStyle)
    mkEdge style (src, dst) = (unId src, unId dst, style)

displayTimeline :: Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
displayTimeline _ entries = do
  items <- mapM mkItem entries
  timelineId <- newIdent
  Timeline.timeline timelineId $ catMaybes items
  where
    mkItem :: (EntryRowR, OptionalSQLData) -> Handler (Maybe Timeline.Item)
    mkItem (entry, _) = do
      public <- isPublic
      render <- getUrlRender
      let mrender url = if public then Nothing else Just (render url)
      let caption = case entry ^. sqlEntryTitle of
            Just t -> t
            Nothing -> "@" <> unId (entry ^. sqlEntryName)
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
                    Timeline._itemGroup = displayKind $ entry ^. sqlEntryKind,
                    Timeline._itemTarget = mrender $ EntryR $ WId $ entry ^. sqlEntryName
                  }

displayGallery :: Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
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

displayLibrary :: Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
displayLibrary _ entries = do
  public <- isPublic
  items <- forM entries $ \e -> runMaybeT $ do
    coverId <- hoistMaybe $ MkId <$> e ^. _2 . optCover
    entry <- hoistLift $ PhotoSwipe.miniatureEntry (e ^. _2 . optMime) Nothing coverId
    let title :: [(Text, Text)] = [("title", t) | t <- toList $ e ^. _1 . sqlEntryTitle]
    caption <- lift $ mkTaskItem public (e ^. _1) (e ^. _2)
    pure $
      entry
        & PhotoSwipe.swpCaption .~ [whamlet|<p *{title}>^{caption}|]
        & PhotoSwipe.swpRedirect .~ (if public then Nothing else Just (EntryR $ WId $ e ^. _1 . sqlEntryName))
  library <- PhotoSwipe.photoswipe (def & PhotoSwipe.swpLibrary .~ True) $ catMaybes items
  pure $ do
    Rcs.entryStyle
    PhotoSwipe.photoswipeHeader
    Rcs.checkboxCode StaticR
    library

displayFuzzy :: Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
displayFuzzy _ entries = do
  items <- forM entries $ Fuse.itemFromEntry . (view sqlEntryName &&& view sqlEntryTitle) . fst
  fuse <- Fuse.widget items
  pure $ do
    Fuse.header
    fuse

displayCalendar :: Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
displayCalendar _ entries = do
  events <- forM entries $ Cal.entryToEvent . fst
  cal <- Cal.widget $ catMaybes events
  pure $ do
    Cal.header
    cal

displayTaskList :: Bool -> [(EntryRowR, OptionalSQLData)] -> Handler Widget
displayTaskList _ entries = do
  public <- isPublic
  items <- mapM (uncurry $ mkTaskItem public) entries
  pure $ do
    Rcs.entryStyle
    Rcs.checkboxCode StaticR
    [whamlet|
      <ul>
        $forall item <- items
          <li>
            ^{item}
    |]

mkTaskItem :: Bool -> EntryRowR -> OptionalSQLData -> Handler Widget
mkTaskItem public entry dat = do
  let i = entry ^. sqlEntryName
  let mAgCount :: Maybe Int = dat ^. optAggregCount >>= fromJSONM
  cb <- Wdgs.checkBoxDWIM i $ dat ^. optTask
  pure
    [whamlet|
    ^{cb}
    #{T.pack " "}
    $maybe agCount <- mAgCount
      <span .aggregate-count>
        #{show agCount}
    $if public
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
