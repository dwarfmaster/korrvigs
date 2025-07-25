{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Web.Search.Results where

import Control.Arrow (second, (***))
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Note (Collection (..))
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

displayEntry :: (EntryRow, Maybe Text) -> Handler Html
displayEntry (entry, title) = do
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
    plain = case title of
      Just t -> [hamlet|#{t}|]
      Nothing -> [hamlet|@#{unId $ entry ^. sqlEntryName}|]

displayResults :: Collection -> Bool -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayResults ColList = displayList
displayResults ColMap = displayMap
displayResults ColGallery = displayGallery
displayResults ColTimeline = displayTimeline
displayResults ColNetwork = displayGraph
displayResults ColFuzzy = displayFuzzy
displayResults ColCalendar = displayCalendar
displayResults ColBiblio = displayUnsupported ColBiblio
displayResults ColKanban = displayUnsupported ColKanban
displayResults ColTaskList = displayTaskList

displayUnsupported :: Collection -> Bool -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayUnsupported col _ _ =
  pure [whamlet|<p>#{show col} is not supported yet|]

displayList :: Bool -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayList _ entries = do
  let entriesWithTitle = second (view optTitle) <$> entries
  entriesH <- mapM displayEntry entriesWithTitle
  pure
    [whamlet|
      <ul>
        $forall entry <- entriesH
          <li>
            #{entry}
    |]

displayMap :: Bool -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayMap _ entries = do
  items <- mapM mkItem entries
  pure $ leafletWidget "resultmap" $ catMaybes items
  where
    mkItem :: (EntryRow, OptionalSQLData) -> Handler (Maybe MapItem)
    mkItem (entry, opt) = case entry ^. sqlEntryGeo of
      Just geom -> do
        html <- displayEntry (entry, opt ^. optTitle)
        pure $
          Just $
            MapItem
              { _mitGeo = geom,
                _mitContent = Just html,
                _mitVar = Nothing
              }
      Nothing -> pure Nothing

displayGraph :: Bool -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayGraph _ entries = do
  nodes <- mapM mkNode entries
  subs <- rSelect $ do
    sub <- selectTable entriesSubTable
    where_ $ sqlElem (sub ^. source) candidates
    where_ $ sqlElem (sub ^. target) candidates
    pure (sub ^. source, sub ^. target)
  refs <- rSelect $ do
    ref <- selectTable entriesRefTable
    where_ $ sqlElem (ref ^. source) candidates
    where_ $ sqlElem (ref ^. target) candidates
    pure (ref ^. source, ref ^. target)
  base <- getBase
  edgeStyle <- Network.defEdgeStyle
  let subStyle = edgeStyle & Network.edgeColor .~ base edgeSubColor
  let refStyle = edgeStyle & Network.edgeColor .~ base edgeRefColor
  let edges = (mkEdge subStyle <$> subs) ++ (mkEdge refStyle <$> refs)
  Network.network "network" nodes edges
  where
    candidates :: O.Field (SqlArray SqlText)
    candidates = sqlArray sqlId $ view (_1 . sqlEntryName) <$> entries
    mkNode :: (EntryRow, OptionalSQLData) -> Handler (Text, Text, Network.NodeStyle)
    mkNode (entry, opt) = do
      public <- isPublic
      style <- Network.defNodeStyle
      render <- getUrlRender
      let mrender url = if public then Nothing else Just (render url)
      base <- getBase
      let caption = case opt ^. optTitle of
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

displayTimeline :: Bool -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayTimeline _ entries = do
  items <- mapM mkItem entries
  timelineId <- newIdent
  Timeline.timeline timelineId $ catMaybes items
  where
    mkItem :: (EntryRow, OptionalSQLData) -> Handler (Maybe Timeline.Item)
    mkItem (entry, opt) = do
      public <- isPublic
      render <- getUrlRender
      let mrender url = if public then Nothing else Just (render url)
      let caption = case opt ^. optTitle of
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

displayGallery :: Bool -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayGallery isCol entries = do
  public <- isPublic
  items <- forM entries $ \e -> case e ^. _2 . optSizeAction of
    Just sizeA -> runMaybeT $ do
      entry <-
        lift
          ( PhotoSwipe.miniatureEntry
              (e ^. _2 . optMime)
              (e ^? _1 . sqlEntryDate . _Just . to zonedTimeToLocalTime . to localDay)
              (e ^. _1 . sqlEntryName)
              sizeA
          )
          >>= hoistMaybe
      pure $ if public then entry & PhotoSwipe.swpRedirect .~ Nothing else entry
    Nothing -> pure Nothing
  gallery <- PhotoSwipe.photoswipe (not isCol) $ catMaybes items
  pure $ do
    PhotoSwipe.photoswipeHeader
    gallery

displayFuzzy :: Bool -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayFuzzy _ entries = do
  items <- forM entries $ Fuse.itemFromEntry . (view sqlEntryName *** view optTitle)
  fuse <- Fuse.widget items
  pure $ do
    Fuse.header
    fuse

displayCalendar :: Bool -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayCalendar _ entries = do
  events <- forM entries $ Cal.entryToEvent . second (view optTitle)
  cal <- Cal.widget $ catMaybes events
  pure $ do
    Cal.header
    cal

displayTaskList :: Bool -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayTaskList _ entries = do
  public <- isPublic
  items <- mapM (uncurry $ mkItem public) entries
  pure $ do
    Rcs.entryStyle
    Rcs.checkboxCode
    [whamlet|
      <ul>
        $forall item <- items
          ^{item}
    |]
  where
    mkItem :: Bool -> EntryRow -> OptionalSQLData -> Handler Widget
    mkItem public entry dat = do
      let i = entry ^. sqlEntryName
      cb <- checkbox i $ dat ^. optTask
      pure
        [whamlet|
        <li>
          ^{cb}
          #{T.pack " "}
          $if public
            ^{plainTitle i (view optTitle dat)}
          $else
            <a href=@{EntryR $ WId i}>
              ^{plainTitle i (view optTitle dat)}
      |]
    plainTitle :: Id -> Maybe Text -> Widget
    plainTitle i title =
      [whamlet|
      $maybe t <- title
        #{t}
      $nothing
        @#{unId i}
    |]
    checkbox :: Id -> Maybe Text -> Handler Widget
    checkbox _ Nothing = pure mempty
    checkbox i (Just tsName) = case parseStatusName tsName of
      Just ts -> do
        (h, w, _) <- Wdgs.checkBox ts (EntryMtdtR $ WId i)
        pure $ w >> toWidget h
      Nothing -> pure mempty
