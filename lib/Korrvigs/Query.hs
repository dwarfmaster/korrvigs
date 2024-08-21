module Korrvigs.Query where

import Control.Lens hiding ((.>))
import Control.Monad
import Data.Default
import Data.Text (Text)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.FTS ((@@?))
import qualified Korrvigs.FTS as FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye
import Opaleye
import Prelude hiding (not)

data JsonTextQuery
  = JSTextLevenshtein
      { _levenshteinText :: Text,
        _levenshteinMax :: Int
      }
  | JSTextEq Text

data JsonNumQuery
  = JSNumEqual Double
  | JSNumLessThan Double
  | JSNumMoreThan Double
  | JSNumWithin Double Double

data JsonObjectQuery
  = JSObjHasSub Text
  | JSObjSubQuery Text JsonQuery

data JsonArrayQuery
  = JSArrLenEq Int
  | JSArrLenLessThan Int
  | JSArrLenMoreThan Int

data JsonBoolQuery
  = JSBoolTrue
  | JSBoolFalse

data JsonTypeQuery
  = JSIsText
  | JSIsNum
  | JSIsObject
  | JSIsArray
  | JSIsBool
  | JSIsNull

data JsonQuery
  = TextQuery JsonTextQuery
  | NumQuery JsonNumQuery
  | ObjectQuery JsonObjectQuery
  | ArrayQuery JsonArrayQuery
  | BoolQuery JsonBoolQuery
  | TypeQuery JsonTypeQuery
  | AnyQuery -- Used to test for the presence of metadata, whatever the value

instance Default JsonQuery where
  def = AnyQuery

data SortCriterion
  = ByDate
  | ByTSRank FTS.Query
  | ByDistanceTo Point
  | ById

instance Default SortCriterion where
  def = ById

data SortOrder
  = SortAsc
  | SortDesc

instance Default SortOrder where
  def = SortAsc

data Query = Query
  { _queryText :: Maybe FTS.Query,
    _queryBefore :: Maybe ZonedTime,
    _queryAfter :: Maybe ZonedTime,
    _queryGeo :: Maybe Polygon,
    _queryDist :: Maybe (Point, Double),
    _queryKind :: Maybe Kind,
    _queryMtdt :: [(Text, JsonQuery)],
    _querySort :: (SortCriterion, SortOrder),
    _queryMaxResults :: Maybe Int
  }

instance Default Query where
  def = Query def def def def def def def def def

makeLenses ''Query

-- Returns the IDs of the matched entries
compile :: Query -> Select EntryRowSQL
compile query = lmt (query ^. queryMaxResults) $ sort (query ^. querySort) $ do
  entry <- selectTable entriesTable
  -- FTS search
  forM_ (query ^. queryText) $ \txt ->
    where_ $ FTS.sqlQuery txt @@? entry ^. sqlEntryText
  -- Check if the entry has a date before the given max time
  forM_ (query ^. queryBefore) $ \dt ->
    let sqlDt = sqlZonedTime dt
     in where_ $ matchNullable (sqlBool False) (.< sqlDt) $ entry ^. sqlEntryDate
  -- Check if the entry has a date after the given min date
  -- Treat lack of duration as duration 0
  forM_ (query ^. queryAfter) $ \dt ->
    let sqlDt = sqlZonedTime dt
     in where_ $ flip (matchNullable $ sqlBool False) (entry ^. sqlEntryDate) $ \entryDate ->
          matchNullable
            (sqlDt .< entryDate)
            (\entryDur -> sqlDt .< addInterval entryDate entryDur)
            (entry ^. sqlEntryDuration)
  -- Check if the entry has a non-empty intersection with the polygon
  forM_ (query ^. queryGeo) $ \poly ->
    let sqlPoly = sqlPolygon poly
     in where_ $ matchNullable (sqlBool False) (stIntersects sqlPoly) $ entry ^. sqlEntryGeo
  -- Check if the entry is within the distance of the given point
  forM_ (query ^. queryDist) $ \(pt, dist) ->
    let sqlPt = sqlPoint pt
     in let sqlDist = sqlDouble dist
         in where_ $
              matchNullable
                (sqlBool False)
                (\p -> stDistance sqlPt p (sqlBool True) .< sqlDist)
                (entry ^. sqlEntryGeo)
  -- Check the kind
  forM_ (query ^. queryKind) $ \kd ->
    where_ $ entry ^. sqlEntryKind .== sqlKind kd
  -- Checks againts metadata
  forM_ (query ^. queryMtdt) $ \q -> do
    mtdt <- selectTable entriesMetadataTable
    where_ $ (mtdt ^. sqlEntry) .== (entry ^. sqlEntryName)
    where_ $ mtdt ^. sqlKey .== sqlStrictText (q ^. _1)
    pure $ compileJsonQuery (q ^. _2) (toNullable $ mtdt ^. sqlValue)
  pure entry
  where
    dir :: (SqlOrd b) => SortOrder -> (a -> Field b) -> Order a
    dir SortAsc = asc
    dir SortDesc = desc
    sort :: (SortCriterion, SortOrder) -> Select EntryRowSQL -> Select EntryRowSQL
    sort (ById, ord) = orderBy $ dir ord $ \e -> e ^. sqlEntryName
    sort (ByTSRank q, ord) =
      orderBy $ dir ord $ \e ->
        matchNullable
          (sqlDouble infinity)
          (FTS.tsRank (FTS.sqlQuery q))
          (e ^. sqlEntryText)
    sort (ByDistanceTo pt, ord) =
      orderBy $ dir ord $ \e ->
        matchNullable
          (sqlDouble infinity)
          (\g -> stDistance (sqlPoint pt) g (sqlBool True))
          (e ^. sqlEntryGeo)
    sort (ByDate, ord) =
      orderBy $ dir ord $ \e ->
        fromNullable
          (sqlZonedTime largeTime)
          (e ^. sqlEntryDate)
    infinity :: Double
    infinity = 1 / 0
    largeTime :: ZonedTime
    largeTime = ZonedTime (LocalTime (fromOrdinalDate 9999 1) (TimeOfDay 0 0 0)) utc
    lmt :: Maybe Int -> Select EntryRowSQL -> Select EntryRowSQL
    lmt = maybe id limit

compileJsonQuery :: JsonQuery -> FieldNullable SqlJsonb -> Field SqlBool
compileJsonQuery AnyQuery _ = sqlBool True
compileJsonQuery (TypeQuery tp) value =
  matchNullable
    (sqlBool False)
    (\v -> sqlJsonTypeof v .== sqlStrictText (tpText tp))
    value
  where
    tpText JSIsText = "string"
    tpText JSIsNum = "number"
    tpText JSIsObject = "object"
    tpText JSIsArray = "array"
    tpText JSIsBool = "boolean"
    tpText JSIsNull = "null"
compileJsonQuery (TextQuery q) value =
  matchNullable (sqlBool False) (compileJsonTextQuery q) (sqlJsonToText value)
compileJsonQuery (NumQuery q) value =
  matchNullable (sqlBool False) (compileJsonNumQuery q) (sqlJsonToNum value)
compileJsonQuery (BoolQuery q) value =
  matchNullable (sqlBool False) (compileJsonBoolQuery q) (sqlJsonToBool value)
compileJsonQuery (ObjectQuery q) value =
  matchNullable (sqlBool False) (compileJsonObjectQuery q) value
compileJsonQuery (ArrayQuery q) value =
  matchNullable (sqlBool False) (compileJsonArrayQuery q) (sqlJsonToArray value)

compileJsonTextQuery :: JsonTextQuery -> Field SqlText -> Field SqlBool
compileJsonTextQuery (JSTextLevenshtein obj dist) value =
  levenshteinLE value (sqlStrictText obj) (sqlInt4 dist) .< sqlInt4 dist
compileJsonTextQuery (JSTextEq txt) value = value .== sqlStrictText txt

compileJsonNumQuery :: JsonNumQuery -> Field SqlFloat8 -> Field SqlBool
compileJsonNumQuery (JSNumEqual d) v = v .== sqlDouble d
compileJsonNumQuery (JSNumLessThan d) v = v .< sqlDouble d
compileJsonNumQuery (JSNumMoreThan d) v = v .> sqlDouble d
compileJsonNumQuery (JSNumWithin mn mx) v = v .> sqlDouble mn .&& v .< sqlDouble mx

compileJsonBoolQuery :: JsonBoolQuery -> Field SqlBool -> Field SqlBool
compileJsonBoolQuery JSBoolTrue = id
compileJsonBoolQuery JSBoolFalse = not

compileJsonObjectQuery :: JsonObjectQuery -> Field SqlJsonb -> Field SqlBool
compileJsonObjectQuery (JSObjHasSub sub) v = v .? sqlStrictText sub
compileJsonObjectQuery (JSObjSubQuery sub q) v =
  compileJsonQuery q $ toNullable v .-> sqlStrictText sub

compileJsonArrayQuery :: JsonArrayQuery -> Field (SqlArray SqlJsonb) -> Field SqlBool
compileJsonArrayQuery (JSArrLenEq i) arr = sqlArrayLength arr 1 .== sqlInt4 i
compileJsonArrayQuery (JSArrLenLessThan i) arr = sqlArrayLength arr 1 .< sqlInt4 i
compileJsonArrayQuery (JSArrLenMoreThan i) arr = sqlArrayLength arr 1 .> sqlInt4 i
