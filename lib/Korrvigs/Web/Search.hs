module Korrvigs.Web.Search (getSearchR) where

import Control.Lens
import Control.Monad (join)
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
import Korrvigs.Monad
import Korrvigs.Query
import Korrvigs.Utils.JSON (sqlJsonToText)
import Korrvigs.Web.Backend
import Korrvigs.Web.Leaflet
import Korrvigs.Web.Login
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import Linear.V2
import Opaleye hiding (Field)
import Text.Julius
import Yesod

sattr :: Text -> Bool -> [(Text, Text)]
sattr attr True = [(attr, "")]
sattr _ False = []

mattr :: Maybe a -> Text -> (a -> Text) -> [(Text, Text)]
mattr (Just x) attr val = [(attr, val x)]
mattr Nothing _ _ = []

ftsForm :: Maybe FTS.Query -> Handler Widget
ftsForm fts =
  pure
    [whamlet|
      <details .search-group *{sattr "open" $ isJust fts}>
        <summary>
          <input type=checkbox name=checkfts *{sattr "checked" $ isJust fts}>
          Full text search
        <input type=text name=fts *{mattr fts "value" FTS.renderQuery}>
    |]

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
  pure $ do
    [whamlet|
      <details .search-group ##{detId} *{sattr "open" $ isJust dist}>
        <summary>
          <input type=checkbox name=checkgeo *{sattr "checked" $ isJust dist}>
          Geometry
        ^{leafletWidget "searchmap" [MapItem (GeoPoint defCenter) Nothing (Just "searchMarker")]}
        <input ##{distId} type=number min=0 name=geodist value=#{defDist}>
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
  where
    defCenter = maybe (V2 2.359775 48.915611) (view _1) dist
    defDist = maybe 1 ((/ 1000.0) . view _2) dist

sortOptions :: [Option (SortCriterion, SortOrder)]
sortOptions = zipWith mkOption [1 ..] opts
  where
    opts = (,) <$> [ById, ByDate] <*> [SortAsc, SortDesc]
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

sortForm :: (SortCriterion, SortOrder) -> Handler Widget
sortForm sopt =
  pure
    [whamlet|
      <select name=sortopts>
        $forall opt <- sortOptions
          <option value=#{optionExternalValue opt} *{sattr "selected" $ optionInternalValue opt == sopt}>
            Sort by:
            #{optionDisplay opt}
    |]

maxResultsOptions :: [Option Int]
maxResultsOptions = mkOption <$> opts
  where
    opts = [10, 25, 50]
    mkOption :: Int -> Option Int
    mkOption n =
      Option
        { optionDisplay = T.pack $ show n,
          optionInternalValue = n,
          optionExternalValue = T.pack $ show n
        }

maxResultsForm :: Maybe Int -> Handler Widget
maxResultsForm n =
  pure
    [whamlet|
      <select name=maxresults>
        $forall opt <- maxResultsOptions
          <option value=#{optionExternalValue opt} *{sattr "selected" $ Just (optionInternalValue opt) == n}>
            Max results:
            #{optionDisplay opt}
    |]

searchForm :: Query -> Handler Widget
searchForm query = do
  fts <- ftsForm $ query ^. queryText
  time <- timeForm (query ^. queryAfter) (query ^. queryBefore)
  kd <- kindForm $ query ^. queryKind
  srt <- sortForm $ query ^. querySort
  mx <- maxResultsForm $ query ^. queryMaxResults
  mtdt <- mtdtForm $ query ^. queryMtdt
  geom <- geoForm $ query ^. queryDist
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

maxResultsField :: Field Handler Int
maxResultsField = selectField $ pure $ mkOptionList maxResultsOptions

getOpt :: (Default a) => Bool -> a -> a
getOpt False _ = def
getOpt True x = x

displayResults :: [(Kind, Id, Maybe (Maybe Text))] -> Handler Widget
displayResults entries =
  pure
    [whamlet|
      <ul>
        $forall (kd,i,title) <- entries
          <li>
            ^{htmlKind kd}
            <a href=@{EntryR $ WId i}>
              $maybe t <- join title
                #{t}
              $nothing
                @#{unId i}
    |]

liftMaybe :: Bool -> (V2 (Maybe Double), Maybe Double) -> Maybe (V2 Double, Double)
liftMaybe True (V2 (Just x) (Just y), Just d) = Just (V2 x y, d * 1000.0)
liftMaybe _ _ = Nothing

getSearchR :: Handler Html
getSearchR = do
  tz <- liftIO getCurrentTimeZone
  let mktz = fmap $ flip ZonedTime tz
  q <-
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
        <*> (fromMaybe def <$> iopt optsField "sortopts")
        <*> iopt maxResultsField "maxresults"
  r <- rSelect $ do
    entry <- compile q
    title <- optional $ limit 1 $ do
      mtdt <- selectTable entriesMetadataTable
      where_ $ (mtdt ^. sqlEntry) .== (entry ^. sqlEntryName)
      where_ $ mtdt ^. sqlKey .== sqlStrictText "title"
      let titleText = sqlJsonToText $ toNullable $ mtdt ^. sqlValue
      pure titleText
    pure (entry ^. sqlEntryKind, entry ^. sqlEntryName, title)
  search <- searchForm q
  results <- displayResults r
  logWrap $ defaultLayout $ search <> results
