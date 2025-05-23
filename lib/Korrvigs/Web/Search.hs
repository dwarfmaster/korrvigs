module Korrvigs.Web.Search (getSearchR) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime
import qualified Korrvigs.FTS as FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Query
import Korrvigs.Web.Backend
import Korrvigs.Web.Leaflet
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Search.Results
import Linear.V2
import Text.Julius
import Yesod

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

applyPrefix :: Maybe Text -> Text -> Text
applyPrefix Nothing nm = nm
applyPrefix (Just prefix) nm = prefix <> "_" <> nm

ftsForm :: Maybe Text -> Maybe FTS.Query -> Handler Widget
ftsForm prefix fts = do
  checkId <- newIdent
  pure $ do
    [whamlet|
      <details .search-group *{sattr "open" $ isJust fts}>
        <summary>
          <input ##{checkId} type=checkbox name=#{applyPrefix prefix "checkfts"} *{sattr "checked" $ isJust fts}>
          Full text search
        <input type=text name=#{applyPrefix prefix "fts"} *{mattr fts "value" FTS.renderQuery}>
    |]
    selectConditional checkId "tsrank-order"

timeForm :: Maybe Text -> Maybe ZonedTime -> Maybe ZonedTime -> Handler Widget
timeForm prefix after before = do
  idAfter <- newIdent
  idBefore <- newIdent
  pure
    [whamlet|
      <details .search-group *{sattr "open" anyJust}>
        <summary>
          <input type=checkbox name=#{applyPrefix prefix "checkdate"} *{sattr "checked" anyJust}>
          Time
        <label for=#{idAfter}>
          After:
        <input id=#{idAfter} type=datetime-local name=#{applyPrefix prefix "after"} *{afterVal}>
        <label for=#{idBefore}>
          Before:
        <input id=#{idBefore} type=datetime-local name=#{applyPrefix prefix "before"} *{beforeVal}>
    |]
  where
    anyJust = isJust after || isJust before
    afterVal = mattr after "value" $ T.pack . iso8601Show . zonedTimeToLocalTime
    beforeVal = mattr before "value" $ T.pack . iso8601Show . zonedTimeToLocalTime

kindForm :: Maybe Text -> Maybe Kind -> Handler Widget
kindForm prefix kd =
  pure
    [whamlet|
    <details .search-group *{sattr "open" $ isJust kd}>
      <summary>
        <input type=checkbox name=#{applyPrefix prefix "checkkind"} *{sattr "checked" $ isJust kd}>
        Kind
      $forall skd <- kinds
        <input type=radio name=#{applyPrefix prefix "kind"} value=#{displayKind skd} *{sattr "checked" $ Just skd == kd}>
          #{displayKind skd}
        <br>
  |]
  where
    kinds :: [Kind]
    kinds = [minBound .. maxBound]

mtdtForm :: Maybe Text -> [(Text, JsonQuery)] -> Handler Widget
mtdtForm prefix qs = do
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
    unless (isJust prefix) $
      toWidget
        [julius|
      const uid = function() {
        return Date.now().toString(36) + Math.random().toString(36).substr(2)
      }
      const rmMetaField = function(i) {
        var tr = document.getElementById(i)
        tr.remove()
      }
      |]
    toWidget
      [julius|
      const #{rawJS $ applyPrefix prefix "addMetaField"} = function() {
        var tr = document.getElementById(#{addId})
        const nid = uid()
        var tds = #{nrowTxt}.replaceAll("{}", nid)
        var ntr = document.createElement("tr")
        ntr.id = nid
        ntr.innerHTML = tds
        tr.parentNode.insertBefore(ntr,tr)
      }
    |]
    let callRm i = "rmMetaField(\"" <> i <> "\")"
    [whamlet|
      <details .search-group *{sattr "open" $ qs /= []}>
        <summary>
          <input type=checkbox name=#{applyPrefix prefix "checkmtdt"} *{sattr "checked" $ qs /= []}>
          Metadata
        <table ##{tId}>
          $forall (i,(key,q)) <- zip qsId qs
            <tr ##{i}>
              <td>
                <input type=text name=#{applyPrefix prefix "mtdtKey"} value=#{key}>
              <td>
                <input type=text name=#{applyPrefix prefix "mtdtVal"} value=#{renderJSQuery q}>
              <td>
                <button type=button onclick=#{callRm i}>
                  -
          <tr ##{addId}>
            <td colspan=3>
              <button type=button onclick=#{applyPrefix prefix "addMetaField"}()>
                +
    |]

geoForm :: Maybe Text -> Maybe (Point, Double) -> Handler Widget
geoForm prefix dist = do
  detId <- newIdent
  distId <- newIdent
  latId <- newIdent
  lngId <- newIdent
  checkId <- newIdent
  pure $ do
    [whamlet|
      <details .search-group ##{detId} *{sattr "open" $ isJust dist}>
        <summary>
          <input ##{checkId} type=checkbox name=#{applyPrefix prefix "checkgeo"} *{sattr "checked" $ isJust dist}>
          Geometry
        ^{leafletWidget mapName [MapItem (GeoPoint defCenter) Nothing (Just markerName)]}
        <input ##{distId} type=number min=0 step=0.25 name=#{applyPrefix prefix "geodist"} value=#{defDist}>
        <input ##{latId} type=hidden name=#{applyPrefix prefix "geolat"} value=#{show $ defCenter ^. _2}>
        <input ##{lngId} type=hidden name=#{applyPrefix prefix "geolng"} value=#{show $ defCenter ^. _1}>
    |]
    toWidget
      [julius|
      function #{rawJS $ applyPrefix prefix "SetupMap"}() {
        document.getElementById(#{detId}).addEventListener("toggle", function() {
          #{mapNameJS}.invalidateSize()
        })
        var searchColor = getComputedStyle(document.body).getPropertyValue('--base08')
        var searchCircle = L.circle(#{rawJS $ jsPoint defCenter}, {
          color: searchColor,
          fillColor: searchColor,
          fillOpacity: 0.5,
          radius: #{rawJS $ show defDist} * 1000
        }).addTo(#{mapNameJS})
        #{mapNameJS}.on('click', function(e) {
          #{rawJS markerName}.setLatLng(e.latlng)
          searchCircle.setLatLng(e.latlng)
          document.getElementById(#{latId}).value = e.latlng.lat
          document.getElementById(#{lngId}).value = e.latlng.lng
        })
        document.getElementById(#{distId}).addEventListener("input", function(e) {
          searchCircle.setRadius(Number(e.target.value) * 1000)
        })
      }
      #{rawJS $ applyPrefix prefix "SetupMap"}()
    |]
    selectConditional checkId "dist-order"
  where
    defCenter = maybe (V2 2.359775 48.915611) (view _1) dist
    defDist = maybe 1 ((/ 1000.0) . view _2) dist
    mapName = applyPrefix prefix "SearchMap"
    mapNameJS = rawJS mapName
    markerName = applyPrefix prefix "SearchMarker"

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
    mkOption DisplayFuzzy = Option "fuzzy" DisplayFuzzy "6"

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

queryRelWidget :: Text -> Text -> Maybe QueryRel -> Handler Widget
queryRelWidget prefix nm qrel = do
  relId <- newIdent
  recId <- newIdent
  query <- queryWidget (Just prefix) $ maybe def (view relOther) qrel
  pure
    [whamlet|
    <details .search-group *{sattr "open" $ isJust qrel}>
      <summary>
        <input ##{relId} type=checkbox name=#{applyPrefix (Just prefix) "check"} *{sattr "checked" $ isJust qrel}>
        #{nm}
      <input ##{recId} type=checkbox name=#{applyPrefix (Just prefix) "rec"} *{sattr "checked" $ maybe False (view relRec) qrel}>
      <label for=#{recId}>
        Recursive
      ^{query}
  |]

queryWidget :: Maybe Text -> Query -> Handler Widget
queryWidget prefix query = do
  fts <- ftsForm prefix $ query ^. queryText
  time <- timeForm prefix (query ^. queryAfter) (query ^. queryBefore)
  geom <- geoForm prefix $ query ^. queryDist
  kd <- kindForm prefix $ query ^. queryKind
  mtdt <- mtdtForm prefix $ query ^. queryMtdt
  (subOf, parentOf) <-
    if isNothing prefix
      then do
        subOf <- queryRelWidget "sub" "Sub of" $ query ^. querySubOf
        parentOf <- queryRelWidget "parent" "Parent of" $ query ^. queryParentOf
        pure (subOf, parentOf)
      else pure (mempty, mempty)
  pure $ do
    fts
    time
    geom
    kd
    mtdt
    subOf
    parentOf

searchForm :: Query -> ResultDisplay -> Handler Widget
searchForm query display = do
  qform <- queryWidget Nothing query
  srt <- sortForm query
  mx <- maxResultsForm $ query ^. queryMaxResults
  disp <- displayResultForm display
  pure $ do
    Rcs.formsStyle
    [whamlet|
      <form action=@{SearchR}>
        ^{qform}
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
kindField = radioField' $ pure $ mkOptionList $ mkOption <$> [minBound .. maxBound]
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

liftMaybe :: Bool -> (V2 (Maybe Double), Maybe Double) -> Maybe (V2 Double, Double)
liftMaybe True (V2 (Just x) (Just y), Just d) = Just (V2 x y, d * 1000.0)
liftMaybe _ _ = Nothing

fixOrder :: Query -> Query
fixOrder q@(Query _ _ _ _ _ _ _ _ _ _ _ _ _ (ByDistanceTo _, _) _) = case q ^. queryDist of
  Just (pt, _) -> q & querySort . _1 .~ ByDistanceTo pt
  Nothing -> q & querySort .~ def
fixOrder q@(Query _ _ _ _ _ _ _ _ _ _ _ _ _ (ByTSRank _, _) _) = case q ^. queryText of
  Just fts -> q & querySort . _1 .~ ByTSRank fts
  Nothing -> q & querySort .~ def
fixOrder q = q

displayFixQuery :: ResultDisplay -> Query -> Query
displayFixQuery DisplayGallery = querySort .~ (ByDate, SortAsc)
displayFixQuery _ = id

fixMaxResults :: Query -> Query
fixMaxResults = queryMaxResults %~ maybe (Just 10) Just

queryForm :: (Maybe LocalTime -> Maybe ZonedTime) -> Maybe Text -> FormInput Handler Query
queryForm mktz prefix =
  Query []
    <$> (getOpt <$> ireq checkBoxField (applyPrefix prefix "checkfts") <*> iopt ftsField (applyPrefix prefix "fts"))
    <*> (getOpt <$> ireq checkBoxField (applyPrefix prefix "checkdate") <*> (mktz <$> iopt datetimeLocalField (applyPrefix prefix "before")))
    <*> (getOpt <$> ireq checkBoxField (applyPrefix prefix "checkdate") <*> (mktz <$> iopt datetimeLocalField (applyPrefix prefix "after")))
    <*> pure Nothing
    <*> ( liftMaybe
            <$> ireq checkBoxField (applyPrefix prefix "checkgeo")
            <*> ( (,)
                    <$> (V2 <$> iopt doubleField (applyPrefix prefix "geolng") <*> iopt doubleField (applyPrefix prefix "geolat"))
                    <*> iopt doubleField (applyPrefix prefix "geodist")
                )
        )
    <*> (getOpt <$> ireq checkBoxField (applyPrefix prefix "checkkind") <*> iopt kindField (applyPrefix prefix "kind"))
    <*> ( getOpt
            <$> ireq checkBoxField (applyPrefix prefix "checkmtdt")
            <*> (zip <$> ireq keysField (applyPrefix prefix "mtdtKey") <*> ireq valuesField (applyPrefix prefix "mtdtVal"))
        )
    <*> pure []
    <*> maybe (queryRelForm mktz "sub") (const $ pure Nothing) prefix
    <*> maybe (queryRelForm mktz "parent") (const $ pure Nothing) prefix
    <*> pure Nothing
    <*> pure Nothing
    <*> (fromMaybe (ByDate, SortDesc) <$> iopt optsField "sortopts")
    <*> (join <$> iopt maxResultsField "maxresults")

queryRelForm :: (Maybe LocalTime -> Maybe ZonedTime) -> Text -> FormInput Handler (Maybe QueryRel)
queryRelForm mktz prefix =
  getOpt
    <$> ireq checkBoxField (applyPrefix (Just prefix) "check")
    <*> ( Just
            <$> ( QueryRel
                    <$> queryForm mktz (Just prefix)
                    <*> ireq checkBoxField (applyPrefix (Just prefix) "rec")
                )
        )

getSearchR :: Handler Html
getSearchR = do
  tz <- liftIO getCurrentTimeZone
  let mktz = fmap $ flip ZonedTime tz
  q' <- runInputGet $ queryForm mktz Nothing
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
