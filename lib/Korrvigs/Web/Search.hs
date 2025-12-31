module Korrvigs.Web.Search (getSearchR, queryForm, displayResultForm) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime
import Korrvigs.Entry
import qualified Korrvigs.FTS as FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Monad.Collections
import Korrvigs.Note.AST (Collection (..))
import Korrvigs.Query
import Korrvigs.Web.Actions
import Korrvigs.Web.Backend
import Korrvigs.Web.JS.Leaflet
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Search.Form
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

titleForm :: Maybe Text -> Maybe Text -> Handler Widget
titleForm prefix title = do
  checkId <- newIdent
  pure $ do
    [whamlet|
      <details .search-group *{sattr "open" $ isJust title}>
        <summary>
          <input ##{checkId} type=checkbox name=#{applyPrefix prefix "checktitle"} *{sattr "checked" $ isJust title}>
          Title regex
        <input type=text name=#{applyPrefix prefix "title"} *{mattr title "value" id}>
    |]

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

kindForm :: Maybe Text -> Maybe KindQuery -> Handler Widget
kindForm prefix kdQ = do
  let fileMimeV = kdQ ^? _Just . _KindQueryFile . queryFileMime . _Just
  fileMimeId <- newIdent
  let synUrlV = kdQ ^? _Just . _KindQuerySyndicate . querySyndicateUrl . _Just
  synUrlId <- newIdent
  pure
    [whamlet|
    <details .search-group *{sattr "open" $ isJust kd}>
      <summary>
        <input type=checkbox name=#{applyPrefix prefix "checkkind"} *{sattr "checked" $ isJust kd}>
        Kind
      <ul>
        <li>
          <input type=radio name=#{applyPrefix prefix "kind"} value=#{displayKind Note} *{sattr "checked" $ Just Note == kd}>
          Note
        <li>
          <input type=radio name=#{applyPrefix prefix "kind"} value=#{displayKind File} *{sattr "checked" $ Just File == kd}>
          File
          <br>
          <label for=#{fileMimeId}>
            Mime:
          <input ##{fileMimeId} type=text name=#{applyPrefix prefix "file-mime"} value=#{fromMaybe "" fileMimeV}>
        <li>
          <input type=radio name=#{applyPrefix prefix "kind"} value=#{displayKind Event} *{sattr "checked" $ Just Event == kd}>
          Event
        <li>
          <input type=radio name=#{applyPrefix prefix "kind"} value=#{displayKind Calendar} *{sattr "checked" $ Just Calendar == kd}>
          Calendar
        <li>
          <input type=radio name=#{applyPrefix prefix "kind"} value=#{displayKind Syndicate} *{sattr "checked" $ Just Syndicate == kd}>
          Syndicate
          <br>
          <label for=#{synUrlId}>
            URL:
          <input ##{synUrlId} type=text name=#{applyPrefix prefix "syn-url"} value=#{fromMaybe "" synUrlV}>
  |]
  where
    kd = queryToKind <$> kdQ

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

incolForm :: Maybe Text -> Maybe QueryInCollection -> Handler Widget
incolForm prefix incol = do
  checkId <- newIdent
  colEntryId <- newIdent
  colNameId <- newIdent
  pure
    [whamlet|
    <details .search-group *{sattr "open" $ isJust incol}>
      <summary>
        <input ##{checkId} type=checkbox name=#{applyPrefix prefix "checkincol"} *{sattr "checked" $ isJust incol}>
        In collection
      <label for=#{colEntryId}>
        Entry:
      <input ##{colEntryId} type=text name=#{applyPrefix prefix "incolentry"} *{mattr incol "value" (unId . view colEntry)}> 
      <label for=#{colNameId}>
        Name:
      <input ##{colNameId} type=text name=#{applyPrefix prefix "incolname"} *{mattr incol "value" (view colName)}>
  |]

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

displayResultForm :: Collection -> Handler Widget
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

showHiddenWidget :: Maybe Text -> Bool -> Handler Widget
showHiddenWidget prefix showHidden = do
  hiddenId <- newIdent
  pure
    [whamlet|
    <input ##{hiddenId} type=checkbox name=#{applyPrefix prefix "showhidden"} *{sattr "checked" showHidden}>
    <label for=#{hiddenId}>
      Show hidden entries
  |]

queryWidget :: Maybe Text -> Query -> Handler Widget
queryWidget prefix query = do
  title <- titleForm prefix $ query ^. queryTitle
  fts <- ftsForm prefix $ query ^. queryText
  time <- timeForm prefix (query ^. queryAfter) (query ^. queryBefore)
  geom <- geoForm prefix $ query ^. queryDist
  kd <- kindForm prefix $ query ^. queryKind
  mtdt <- mtdtForm prefix $ query ^. queryMtdt
  incol <- incolForm prefix $ query ^. queryInCollection
  showHidden <- showHiddenWidget prefix $ query ^. queryShowHidden
  (subOf, parentOf, mentioning, mentioned) <-
    if isNothing prefix
      then do
        subOf <- queryRelWidget "sub" "Sub of" $ query ^. querySubOf
        parentOf <- queryRelWidget "parent" "Parent of" $ query ^. queryParentOf
        mentioning <- queryRelWidget "mentioning" "Mentioning" $ query ^. queryMentioning
        mentioned <- queryRelWidget "mentioned" "Mentioned by" $ query ^. queryMentionedBy
        pure (subOf, parentOf, mentioning, mentioned)
      else pure (mempty, mempty, mempty, mempty)
  pure $ do
    showHidden
    title
    fts
    time
    geom
    kd
    mtdt
    incol
    subOf
    parentOf
    mentioning
    mentioned

searchForm :: Query -> Collection -> Handler Widget
searchForm query display = do
  qform <- queryWidget Nothing query
  srt <- sortForm query
  mx <- maxResultsForm $ query ^. queryMaxResults
  disp <- displayResultForm display
  pure $ do
    Rcs.formsStyle
    [whamlet|
      <form #query-form action=@{SearchR}>
        ^{qform}
        ^{srt}
        ^{mx}
        ^{disp}
        <input .search-button type=submit value="Search">
    |]

fixOrder :: Query -> Query
fixOrder q@(Query _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (ByDistanceTo _, _) _) = case q ^. queryDist of
  Just (pt, _) -> q & querySort . _1 .~ ByDistanceTo pt
  Nothing -> q & querySort .~ def
fixOrder q@(Query _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (ByTSRank _, _) _) = case q ^. queryText of
  Just fts -> q & querySort . _1 .~ ByTSRank fts
  Nothing -> q & querySort .~ def
fixOrder q = q

displayFixQuery :: Collection -> Query -> Query
displayFixQuery ColGallery = querySort .~ (ByDate, SortAsc)
displayFixQuery _ = id

fixMaxResults :: Query -> Query
fixMaxResults = queryMaxResults %~ maybe (Just 10) Just

getSearchR :: Handler Html
getSearchR = do
  public <- isPublic
  tz <- liftIO getCurrentTimeZone
  let mktz = fmap $ flip ZonedTime tz
  q' <- runInputGet $ queryForm mktz Nothing
  display <- runInputGet $ displayForm ColList
  hasMaxResults <- isJust <$> lookupGetParam "maxresults"
  let q = displayFixQuery display $ (if hasMaxResults then id else fixMaxResults) $ fixOrder q'
  search <- searchForm q display
  actions <- actionsWidget $ TargetSearch q display
  results <- displayResults display False =<< runQuery display q
  defaultLayout $ do
    setTitle "Korrvigs search"
    setDescriptionIdemp "Korrvigs search page"
    unless public $ do
      search
      actions
    [whamlet|<div .search-results> ^{results}|]
