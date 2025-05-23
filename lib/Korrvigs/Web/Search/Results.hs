{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Web.Search.Results where

import Control.Arrow (second)
import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Compute.Action
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Query
import Korrvigs.Utils.Time
import Korrvigs.Web.Backend
import Korrvigs.Web.Leaflet
import qualified Korrvigs.Web.PhotoSwipe as PhotoSwipe
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import qualified Korrvigs.Web.Vis.Network as Network
import qualified Korrvigs.Web.Vis.Timeline as Timeline
import Opaleye hiding (Field)
import qualified Opaleye as O
import Yesod

data ResultDisplay
  = DisplayList
  | DisplayMap
  | DisplayGraph
  | DisplayTimeline
  | DisplayGallery
  deriving (Eq, Ord, Enum, Bounded)

data OptionalSQLDataImpl a b = OptionalSQLData
  { _optTitle :: a,
    _optSizeAction :: b
  }

makeLenses ''OptionalSQLDataImpl
$(makeAdaptorAndInstanceInferrable "pOptSQLData" ''OptionalSQLDataImpl)

type OptionalSQLData = OptionalSQLDataImpl (Maybe Text) (Maybe Action)

type OptionalSQLDataSQL = OptionalSQLDataImpl (FieldNullable SqlText) (FieldNullable SqlJsonb)

instance Default OptionalSQLData where
  def = OptionalSQLData Nothing Nothing

instance Default OptionalSQLDataSQL where
  def = OptionalSQLData O.null O.null

optDef :: OptionalSQLDataSQL
optDef = def

otherQuery :: ResultDisplay -> EntryRowSQL -> Select OptionalSQLDataSQL
otherQuery display entry = case display of
  DisplayGallery -> do
    void $ selComp (entry ^. sqlEntryName) "miniature"
    sz <- selComp (entry ^. sqlEntryName) "size"
    pure $ optDef & optSizeAction .~ toNullable (sz ^. sqlCompAction)
  DisplayGraph -> pure optDef
  _ -> do
    title <- selectTextMtdt Title $ entry ^. sqlEntryName
    pure $ optDef & optTitle .~ title

runQuery :: ResultDisplay -> Query -> Handler [(EntryRow, OptionalSQLData)]
runQuery display query = rSelect $ do
  entry <- compile query
  other <- otherQuery display entry
  pure (entry, other)

expandIDs :: ResultDisplay -> [Id] -> Handler [(EntryRow, OptionalSQLData)]
expandIDs display ids = do
  res <- forM ids $ \i -> rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    other <- otherQuery display entry
    pure (entry, other)
  pure $ catMaybes res

displayEntry :: (EntryRow, Maybe Text) -> Handler Html
displayEntry (entry, title) = do
  kd <- htmlKind' $ entry ^. sqlEntryKind
  [hamlet|
    #{kd}
    <a href=@{EntryR $ WId $ entry ^. sqlEntryName}>
      $maybe t <- title
        #{t}
      $nothing
        @#{unId $ entry ^. sqlEntryName}
  |]
    <$> getUrlRenderParams

displayResults :: ResultDisplay -> [(EntryRow, OptionalSQLData)] -> Handler Widget
displayResults DisplayList entries = do
  let entriesWithTitle = second (view optTitle) <$> entries
  entriesH <- mapM displayEntry entriesWithTitle
  pure
    [whamlet|
      <ul>
        $forall entry <- entriesH
          <li>
            #{entry}
    |]
displayResults DisplayMap entries = do
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
displayResults DisplayGraph entries = do
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
      style <- Network.defNodeStyle
      render <- getUrlRender
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
            & Network.nodeLink ?~ render (EntryR $ WId $ entry ^. sqlEntryName)
        )
    mkEdge :: Network.EdgeStyle -> (Id, Id) -> (Text, Text, Network.EdgeStyle)
    mkEdge style (src, dst) = (unId src, unId dst, style)
displayResults DisplayTimeline entries = do
  items <- mapM mkItem entries
  timelineId <- newIdent
  Timeline.timeline timelineId $ catMaybes items
  where
    mkItem :: (EntryRow, OptionalSQLData) -> Handler (Maybe Timeline.Item)
    mkItem (entry, opt) = do
      render <- getUrlRender
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
                    Timeline._itemTarget = Just $ render $ EntryR $ WId $ entry ^. sqlEntryName
                  }
displayResults DisplayGallery entries = do
  items <- forM entries $ \e -> case e ^. _2 . optSizeAction of
    Just sizeA ->
      PhotoSwipe.miniatureEntry
        (e ^? _1 . sqlEntryDate . _Just . to zonedTimeToLocalTime . to localDay)
        (e ^. _1 . sqlEntryName)
        sizeA
    Nothing -> pure Nothing
  gallery <- PhotoSwipe.photoswipe $ catMaybes items
  pure $ do
    PhotoSwipe.photoswipeHeader
    gallery
