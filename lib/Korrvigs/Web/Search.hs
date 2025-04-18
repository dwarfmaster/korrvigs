{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Web.Search (getSearchR) where

import Control.Arrow (second)
import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime
import Korrvigs.Compute.Action
import Korrvigs.Entry
import qualified Korrvigs.FTS as FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Query
import Korrvigs.Utils.Time
import Korrvigs.Web.Backend
import Korrvigs.Web.Leaflet
import qualified Korrvigs.Web.PhotoSwipe as PhotoSwipe
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import qualified Korrvigs.Web.Vis.Network as Network
import qualified Korrvigs.Web.Vis.Timeline as Timeline
import Linear.V2
import Opaleye hiding (Field)
import qualified Opaleye as O
import Text.Julius
import Yesod

data ResultDisplay
  = DisplayList
  | DisplayMap
  | DisplayGraph
  | DisplayTimeline
  | DisplayGallery
  deriving (Eq, Ord, Enum, Bounded)

sattr :: Text -> Bool -> [(Text, Text)]
sattr attr True = [(attr, "")]
sattr _ False = []

mattr :: Maybe a -> Text -> (a -> Text) -> [(Text, Text)]
mattr (Just x) attr val = [(attr, val x)]
mattr Nothing _ _ = []

selectConditional :: Text -> Text -> Widget
selectConditional checkId cls =
  toWidget
    [julius|
  document.getElementById(#{checkId}).addEventListener("change", function(ev) {
    for (let e of document.getElementsByClassName(#{cls})) {
      if (ev.currentTarget.checked) {
        e.removeAttribute("disabled")
      } else {
        e.setAttribute("disabled", "")
        var select = document.getElementById("sortSelId")
        if (select.value == e.value) {
          select.selectedIndex = 1
        }
      }
    }
  })
|]

ftsForm :: Maybe FTS.Query -> Handler Widget
ftsForm fts = do
  checkId <- newIdent
  pure $ do
    [whamlet|
      <details .search-group *{sattr "open" $ isJust fts}>
        <summary>
          <input ##{checkId} type=checkbox name=checkfts *{sattr "checked" $ isJust fts}>
          Full text search
        <input type=text name=fts *{mattr fts "value" FTS.renderQuery}>
    |]
    selectConditional checkId "tsrank-order"

timeForm :: Maybe ZonedTime -> Maybe ZonedTime -> Handler Widget
timeForm after before = do
  idAfter <- newIdent
  idBefore <- newIdent
  pure
    [whamlet|
      <details .search-group *{sattr "open" anyJust}>
        <summary>
          <input type=checkbox name=checkdate *{sattr "checked" anyJust}>
          Time
        <label for=#{idAfter}>
          After:
        <input id=#{idAfter} type=datetime-local name=after *{afterVal}>
        <label for=#{idBefore}>
          Before:
        <input id=#{idBefore} type=datetime-local name=before *{beforeVal}>
    |]
  where
    anyJust = isJust after || isJust before
    afterVal = mattr after "value" $ T.pack . iso8601Show . zonedTimeToLocalTime
    beforeVal = mattr before "value" $ T.pack . iso8601Show . zonedTimeToLocalTime

kindForm :: Maybe Kind -> Handler Widget
kindForm kd =
  pure
    [whamlet|
    <details .search-group *{sattr "open" $ isJust kd}>
      <summary>
        <input type=checkbox name=checkkind *{sattr "checked" $ isJust kd}>
        Kind
      $forall skd <- kinds
        <input type=radio name=kind value=#{displayKind skd} *{sattr "checked" $ Just skd == kd}>
          #{displayKind skd}
        <br>
  |]
  where
    kinds :: [Kind]
    kinds = [minBound .. maxBound]

mtdtForm :: [(Text, JsonQuery)] -> Handler Widget
mtdtForm qs = do
  tId <- newIdent
  addId <- newIdent
  qsId <- sequence $ newIdent <$ qs
  pure $ do
    let nrowTxt :: Text =
          mconcat
            [ "<td><input type=\"text\" name=\"mtdtKey\" /></td>",
              "<td><input type=\"text\" name=\"mtdtVal\" /></td>",
              "<td><button type=\"button\" onclick='rmMetaField(\"{}\")'>-</button></td>"
            ]
    toWidget
      [julius|
      const uid = function() {
        return Date.now().toString(36) + Math.random().toString(36).substr(2)
      }
      const addMetaField = function() {
        var tr = document.getElementById(#{addId})
        const nid = uid()
        var tds = #{nrowTxt}.replaceAll("{}", nid)
        var ntr = document.createElement("tr")
        console.log(nid)
        ntr.id = nid
        ntr.innerHTML = tds
        tr.parentNode.insertBefore(ntr,tr)
      }
      const rmMetaField = function(i) {
        var tr = document.getElementById(i)
        tr.remove()
      }
    |]
    let callRm i = "rmMetaField(\"" <> i <> "\")"
    [whamlet|
      <details .search-group *{sattr "open" $ qs /= []}>
        <summary>
          <input type=checkbox name=checkmtdt *{sattr "checked" $ qs /= []}>
          Metadata
        <table ##{tId}>
          $forall (i,(key,q)) <- zip qsId qs
            <tr ##{i}>
              <td>
                <input type=text name=mtdtKey value=#{key}>
              <td>
                <input type=text name=mtdtVal value=#{renderJSQuery q}>
              <td>
                <button type=button onclick=#{callRm i}>
                  -
          <tr ##{addId}>
            <td colspan=3>
              <button type=button onclick=addMetaField()>
                +
    |]

geoForm :: Maybe (Point, Double) -> Handler Widget
geoForm dist = do
  detId <- newIdent
  distId <- newIdent
  latId <- newIdent
  lngId <- newIdent
  checkId <- newIdent
  pure $ do
    [whamlet|
      <details .search-group ##{detId} *{sattr "open" $ isJust dist}>
        <summary>
          <input ##{checkId} type=checkbox name=checkgeo *{sattr "checked" $ isJust dist}>
          Geometry
        ^{leafletWidget "searchmap" [MapItem (GeoPoint defCenter) Nothing (Just "searchMarker")]}
        <input ##{distId} type=number min=0 step=0.25 name=geodist value=#{defDist}>
        <input ##{latId} type=hidden name=geolat value=#{show $ defCenter ^. _2}>
        <input ##{lngId} type=hidden name=geolng value=#{show $ defCenter ^. _1}>
    |]
    toWidget
      [julius|
      document.getElementById(#{detId}).addEventListener("toggle", function() {
        searchmap.invalidateSize()
      })
      var searchColor = getComputedStyle(document.body).getPropertyValue('--base08')
      var searchCircle = L.circle(#{rawJS $ jsPoint defCenter}, {
        color: searchColor,
        fillColor: searchColor,
        fillOpacity: 0.5,
        radius: #{rawJS $ show defDist} * 1000
      }).addTo(searchmap)
      searchmap.on('click', function(e) {
        searchMarker.setLatLng(e.latlng)
        searchCircle.setLatLng(e.latlng)
        document.getElementById(#{latId}).value = e.latlng.lat
        document.getElementById(#{lngId}).value = e.latlng.lng
      })
      document.getElementById(#{distId}).addEventListener("input", function(e) {
        searchCircle.setRadius(Number(e.target.value) * 1000)
      })
    |]
    selectConditional checkId "dist-order"
  where
    defCenter = maybe (V2 2.359775 48.915611) (view _1) dist
    defDist = maybe 1 ((/ 1000.0) . view _2) dist

sortOptions :: [Option (SortCriterion, SortOrder)]
sortOptions = zipWith mkOption [1 ..] opts
  where
    opts = (,) <$> [ById, ByDate, ByDistanceTo (V2 0 0), ByTSRank (FTS.Phrase [])] <*> [SortAsc, SortDesc]
    mkOption :: Int -> (SortCriterion, SortOrder) -> Option (SortCriterion, SortOrder)
    mkOption i (crit, order) =
      Option
        { optionDisplay = dCrit crit <> " " <> dOrd order,
          optionInternalValue = (crit, order),
          optionExternalValue = T.pack $ show i
        }
    dCrit ByDate = "Date"
    dCrit ById = "Id"
    dCrit (ByTSRank _) = "Text rank"
    dCrit (ByDistanceTo _) = "Distance"
    dOrd SortAsc = "ascending"
    dOrd SortDesc = "descending"

sortForm :: Query -> Handler Widget
sortForm q =
  pure
    [whamlet|
      <select #sortSelId name=sortopts>
        $forall opt <- sortOptions
          <option value=#{optionExternalValue opt} *{sattr "selected" $ optionInternalValue opt == sopt} *{disableAttrs opt}>
            Sort by:
            #{optionDisplay opt}
    |]
  where
    sopt = q ^. querySort
    disableAttrs :: Option (SortCriterion, SortOrder) -> [(Text, Text)]
    disableAttrs opt = case optionInternalValue opt ^. _1 of
      ByDistanceTo _ -> [("disabled", "") | isNothing $ q ^. queryDist] ++ [("class", "dist-order")]
      ByTSRank _ -> [("disabled", "") | isNothing $ q ^. queryText] ++ [("class", "tsrank-order")]
      _ -> []

maxResultsOptions :: [Option (Maybe Int)]
maxResultsOptions = mkOption <$> opts
  where
    opts = [Just 10, Just 25, Just 50, Nothing]
    mkOption :: Maybe Int -> Option (Maybe Int)
    mkOption (Just n) =
      Option
        { optionDisplay = T.pack $ show n,
          optionInternalValue = Just n,
          optionExternalValue = T.pack $ show n
        }
    mkOption Nothing =
      Option
        { optionDisplay = "all",
          optionInternalValue = Nothing,
          optionExternalValue = "nothing"
        }

maxResultsForm :: Maybe Int -> Handler Widget
maxResultsForm n =
  pure
    [whamlet|
      <select name=maxresults>
        $forall opt <- maxResultsOptions
          <option value=#{optionExternalValue opt} *{sattr "selected" $ optionInternalValue opt == n}>
            Max results:
            #{optionDisplay opt}
    |]

displayResultOptions :: [Option ResultDisplay]
displayResultOptions = mkOption <$> [minBound .. maxBound]
  where
    mkOption :: ResultDisplay -> Option ResultDisplay
    mkOption DisplayList = Option "list" DisplayList "1"
    mkOption DisplayMap = Option "map" DisplayMap "2"
    mkOption DisplayGraph = Option "graph" DisplayGraph "3"
    mkOption DisplayTimeline = Option "timeline" DisplayTimeline "4"
    mkOption DisplayGallery = Option "gallery" DisplayGallery "5"

displayResultForm :: ResultDisplay -> Handler Widget
displayResultForm display =
  pure
    [whamlet|
    <select name=display>
      $forall opt <- displayResultOptions
        <option value=#{optionExternalValue opt} *{sattr "selected" $ optionInternalValue opt == display}>
          Display:
          #{optionDisplay opt}
  |]

searchForm :: Query -> ResultDisplay -> Handler Widget
searchForm query display = do
  fts <- ftsForm $ query ^. queryText
  time <- timeForm (query ^. queryAfter) (query ^. queryBefore)
  kd <- kindForm $ query ^. queryKind
  srt <- sortForm query
  mx <- maxResultsForm $ query ^. queryMaxResults
  mtdt <- mtdtForm $ query ^. queryMtdt
  geom <- geoForm $ query ^. queryDist
  disp <- displayResultForm display
  pure $ do
    Rcs.formsStyle
    [whamlet|
      <form action=@{SearchR}>
        ^{fts}
        ^{time}
        ^{geom}
        ^{kd}
        ^{mtdt}
        ^{srt}
        ^{mx}
        ^{disp}
        <input .search-button type=submit value="Search">
    |]

ftsField :: Field Handler FTS.Query
ftsField =
  Field
    { fieldParse = \rawVals _ ->
        case rawVals of
          [""] -> pure $ Right Nothing
          [query] -> pure $ case FTS.parseQuery query of
            Left err -> Left $ SomeMessage err
            Right q -> Right $ Just q
          _ -> pure $ Right Nothing,
      fieldView = \idAttr nameAttr otherAttrs _ _ ->
        [whamlet|<input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=text>|],
      fieldEnctype = UrlEncoded
    }

keysField :: Field Handler [Text]
keysField =
  Field
    { fieldParse = \rawVals _ -> pure $ Right $ Just rawVals,
      fieldView = \_ _ _ _ _ -> mempty,
      fieldEnctype = UrlEncoded
    }

valuesField :: Field Handler [JsonQuery]
valuesField =
  Field
    { fieldParse = \rawVals _ ->
        case mapM parseJSQuery rawVals of
          Left err -> pure $ Left $ SomeMessage err
          Right qs -> pure $ Right $ Just qs,
      fieldView = \_ _ _ _ _ -> mempty,
      fieldEnctype = UrlEncoded
    }

kindField :: Field Handler Kind
kindField = radioField $ pure $ mkOptionList $ mkOption <$> [minBound .. maxBound]
  where
    mkOption kd =
      Option
        { optionDisplay = displayKind kd,
          optionInternalValue = kd,
          optionExternalValue = displayKind kd
        }

optsField :: Field Handler (SortCriterion, SortOrder)
optsField = selectField $ pure $ mkOptionList sortOptions

maxResultsField :: Field Handler (Maybe Int)
maxResultsField = selectField $ pure $ mkOptionList maxResultsOptions

displayResultsField :: Field Handler ResultDisplay
displayResultsField = selectField $ pure $ mkOptionList displayResultOptions

getOpt :: (Default a) => Bool -> a -> a
getOpt False _ = def
getOpt True x = x

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

runQuery :: ResultDisplay -> Query -> Handler [(EntryRow, OptionalSQLData)]
runQuery display query = rSelect $ do
  entry <- compile query
  other <- case display of
    DisplayGallery -> do
      void $ selComp (entry ^. sqlEntryName) "miniature"
      sz <- selComp (entry ^. sqlEntryName) "size"
      pure $ optDef & optSizeAction .~ toNullable (sz ^. sqlCompAction)
    DisplayGraph -> pure optDef
    _ -> do
      title <- selectTextMtdt Title $ entry ^. sqlEntryName
      pure $ optDef & optTitle .~ title
  pure (entry, other)

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

liftMaybe :: Bool -> (V2 (Maybe Double), Maybe Double) -> Maybe (V2 Double, Double)
liftMaybe True (V2 (Just x) (Just y), Just d) = Just (V2 x y, d * 1000.0)
liftMaybe _ _ = Nothing

fixOrder :: Query -> Query
fixOrder q@(Query _ _ _ _ _ _ _ (ByDistanceTo _, _) _) = case q ^. queryDist of
  Just (pt, _) -> q & querySort . _1 .~ ByDistanceTo pt
  Nothing -> q & querySort .~ def
fixOrder q@(Query _ _ _ _ _ _ _ (ByTSRank _, _) _) = case q ^. queryText of
  Just fts -> q & querySort . _1 .~ ByTSRank fts
  Nothing -> q & querySort .~ def
fixOrder q = q

displayFixQuery :: ResultDisplay -> Query -> Query
displayFixQuery DisplayGallery = querySort .~ (ByDate, SortAsc)
displayFixQuery _ = id

fixMaxResults :: Query -> Query
fixMaxResults = queryMaxResults %~ maybe (Just 10) Just

getSearchR :: Handler Html
getSearchR = do
  tz <- liftIO getCurrentTimeZone
  let mktz = fmap $ flip ZonedTime tz
  q' <-
    runInputGet $
      Query
        <$> (getOpt <$> ireq checkBoxField "checkfts" <*> iopt ftsField "fts")
        <*> (getOpt <$> ireq checkBoxField "checkdate" <*> (mktz <$> iopt datetimeLocalField "before"))
        <*> (getOpt <$> ireq checkBoxField "checkdate" <*> (mktz <$> iopt datetimeLocalField "after"))
        <*> pure Nothing
        <*> ( liftMaybe
                <$> ireq checkBoxField "checkgeo"
                <*> ( (,)
                        <$> (V2 <$> iopt doubleField "geolng" <*> iopt doubleField "geolat")
                        <*> iopt doubleField "geodist"
                    )
            )
        <*> (getOpt <$> ireq checkBoxField "checkkind" <*> iopt kindField "kind")
        <*> ( getOpt
                <$> ireq checkBoxField "checkmtdt"
                <*> (zip <$> ireq keysField "mtdtKey" <*> ireq valuesField "mtdtVal")
            )
        <*> (fromMaybe (ByDate, SortDesc) <$> iopt optsField "sortopts")
        <*> (join <$> iopt maxResultsField "maxresults")
  display <- runInputGet $ fromMaybe DisplayList <$> iopt displayResultsField "display"
  hasMaxResults <- isJust <$> lookupGetParam "maxresults"
  let q = displayFixQuery display $ (if hasMaxResults then id else fixMaxResults) $ fixOrder q'
  search <- searchForm q display
  results <- displayResults display =<< runQuery display q
  defaultLayout $ do
    setTitle "Korrvigs search"
    setDescriptionIdemp "Korrvigs search page"
    search
    [whamlet|<div .search-results> ^{results}|]
