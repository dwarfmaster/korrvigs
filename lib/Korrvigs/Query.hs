{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Query where

import Control.Lens hiding (noneOf, (.>))
import Control.Monad
import Data.Default
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.FTS ((@@?))
import qualified Korrvigs.FTS as FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Metadata.Collections
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye
import Opaleye
import Text.Parsec
import Text.Parsec.Number
import Prelude hiding (not)

-- _____        __ _       _ _   _

-- |  _ \  ___ / _(_)_ __ (_) |_(_) ___  _ __
-- | | | |/ _ \ |_| | '_ \| | __| |/ _ \| '_ \
-- | |_| |  __/  _| | | | | | |_| | (_) | | | |
-- |____/ \___|_| |_|_| |_|_|\__|_|\___/|_| |_|
data JsonTextQuery
  = JSTextLevenshtein
      { _levenshteinText :: Text,
        _levenshteinMax :: Int
      }
  | JSTextEq Text
  deriving (Eq, Show)

data JsonNumQuery
  = JSNumEqual Double
  | JSNumLessThan Double
  | JSNumMoreThan Double
  | JSNumWithin Double Double
  deriving (Eq, Show)

data JsonObjectQuery
  = JSObjHasSub Text
  | JSObjSubQuery Text JsonQuery
  deriving (Eq, Show)

data JsonArrayQuery
  = JSArrLenEq Int
  | JSArrLenLessThan Int
  | JSArrLenMoreThan Int
  deriving (Eq, Show)

data JsonBoolQuery
  = JSBoolTrue
  | JSBoolFalse
  deriving (Eq, Show)

data JsonTypeQuery
  = JSIsText
  | JSIsNum
  | JSIsObject
  | JSIsArray
  | JSIsBool
  | JSIsNull
  deriving (Eq, Show)

data JsonQuery
  = TextQuery JsonTextQuery
  | NumQuery JsonNumQuery
  | ObjectQuery JsonObjectQuery
  | ArrayQuery JsonArrayQuery
  | BoolQuery JsonBoolQuery
  | TypeQuery JsonTypeQuery
  | AnyQuery -- Used to test for the presence of metadata, whatever the value
  deriving (Eq, Show)

instance Default JsonQuery where
  def = AnyQuery

data SortCriterion
  = ByDate
  | ByTSRank FTS.Query
  | ByDistanceTo Point
  | ById
  deriving (Eq)

instance Default SortCriterion where
  def = ById

data SortOrder
  = SortAsc
  | SortDesc
  deriving (Eq)

instance Default SortOrder where
  def = SortAsc

data QueryRel = QueryRel
  { _relOther :: Query,
    _relRec :: Bool
  }

data Query = Query
  { _queryText :: Maybe FTS.Query,
    _queryBefore :: Maybe ZonedTime,
    _queryAfter :: Maybe ZonedTime,
    _queryGeo :: Maybe Polygon,
    _queryDist :: Maybe (Point, Double),
    _queryKind :: Maybe Kind,
    _queryMtdt :: [(Text, JsonQuery)],
    _queryInCollection :: [[Text]],
    _querySubOf :: Maybe QueryRel,
    _queryParentOf :: Maybe QueryRel,
    _queryMentioning :: Maybe QueryRel,
    _queryMentionedBy :: Maybe QueryRel,
    _querySort :: (SortCriterion, SortOrder),
    _queryMaxResults :: Maybe Int
  }

instance Default Query where
  def = Query def def def def def def def def def def def def def def

makeLenses ''Query
makeLenses ''QueryRel

-- Match all that are related by the relation table to the result of the query
compileRel :: EntryRowSQL -> Table a RelRowSQL -> Bool -> QueryRel -> Select ()
compileRel entry tbl direct q = do
  otherEntry <- compile $ q ^. relOther
  candidate <- closure (selectTable tbl) src tgt $ otherEntry ^. sqlEntryName
  where_ $ entry ^. sqlEntryName .== candidate
  where
    src = if direct then view source else view target
    tgt = if direct then view target else view source
    closure = if q ^. relRec then transitiveClosure else transitiveClosureStep

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
    where_ $ compileJsonQuery (q ^. _2) (toNullable $ mtdt ^. sqlValue)
  -- Check collections
  forM_ (query ^. queryInCollection) $ \col -> do
    (colEntry, _) <- selectCol MiscCollection col False
    where_ $ colEntry .== entry ^. sqlEntryName
  -- Relations
  forM_ (query ^. querySubOf) $ compileRel entry entriesSubTable False
  forM_ (query ^. queryParentOf) $ compileRel entry entriesSubTable True
  forM_ (query ^. queryMentioning) $ compileRel entry entriesRefTable False
  forM_ (query ^. queryMentionedBy) $ compileRel entry entriesRefTable True
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

-- _____

-- |  _ \ __ _ _ __ ___  ___ _ __
-- | |_) / _` | '__/ __|/ _ \ '__|
-- |  __/ (_| | |  \__ \  __/ |
-- |_|   \__,_|_|  |___/\___|_|

-- Syntax of metadata query:
--   |key1|key2|key3?query for sub queries
--   key to check for key presence
--   # for num queries
--   : for type queries
--   "" for text queries
--   = for bool queries
--   [] for array queries

parseJSQuery :: Text -> Either Text JsonQuery
parseJSQuery txt = case parse queryP "<jsquery>" txt of
  Left err -> Left . T.pack $ show err
  Right q -> Right q

parseMtdtQuery :: Text -> Either Text (Text, JsonQuery)
parseMtdtQuery txt = case parse mtdtQueryP "<mtdtquery>" txt of
  Left err -> Left . T.pack $ show err
  Right q -> Right q

mtdtQueryP :: (Stream s Identity Char) => Parsec s u (Text, JsonQuery)
mtdtQueryP =
  (,)
    <$> (T.pack <$> many1 (noneOf "|?"))
    <*> queryP

queryP :: (Stream s Identity Char) => Parsec s u JsonQuery
queryP =
  char '|' *> (ObjectQuery <$> subQueryP)
    <|> char '?' *> leafQueryP <* eof

subQueryP :: (Stream s Identity Char) => Parsec s u JsonObjectQuery
subQueryP = do
  key <- T.pack <$> many1 (noneOf "|?")
  JSObjSubQuery key <$> queryP

leafQueryP :: (Stream s Identity Char) => Parsec s u JsonQuery
leafQueryP =
  do
    char '#' *> (NumQuery <$> numQueryP)
    <|> char ':' *> (TypeQuery <$> typeQueryP)
    <|> char '"' *> (TextQuery <$> textQueryP)
    <|> char '=' *> (BoolQuery <$> boolQueryP)
    <|> char '[' *> (ArrayQuery <$> arrayQueryP)
    <|> (ObjectQuery <$> hasKeyP)
    <|> pure AnyQuery

numQueryP :: (Stream s Identity Char) => Parsec s u JsonNumQuery
numQueryP =
  char '=' *> (JSNumEqual <$> floating2 True)
    <|> char '<' *> (JSNumLessThan <$> floating2 True)
    <|> char '>' *> (JSNumMoreThan <$> floating2 True)
    <|> ( do
            void $ char '['
            mn <- floating2 True
            void $ char ':'
            mx <- floating2 True
            void $ char ']'
            pure $ JSNumWithin mn mx
        )

typeQueryP :: (Stream s Identity Char) => Parsec s u JsonTypeQuery
typeQueryP =
  string "text" $> JSIsText
    <|> string "number" $> JSIsNum
    <|> string "object" $> JSIsObject
    <|> string "array" $> JSIsArray
    <|> string "bool" $> JSIsBool
    <|> string "null" $> JSIsNull

textQueryP :: (Stream s Identity Char) => Parsec s u JsonTextQuery
textQueryP = do
  txt <- T.pack <$> many (noneOf "\"")
  void $ char '"'
  mx <- optionMaybe $ char '<' *> decimal
  pure $ case mx of
    Just m -> JSTextLevenshtein txt m
    Nothing -> JSTextEq txt

boolQueryP :: (Stream s Identity Char) => Parsec s u JsonBoolQuery
boolQueryP =
  string "true" $> JSBoolTrue
    <|> string "false" $> JSBoolFalse

arrayQueryP :: (Stream s Identity Char) => Parsec s u JsonArrayQuery
arrayQueryP = do
  a <-
    char '=' *> (JSArrLenEq <$> decimal)
      <|> char '<' *> (JSArrLenLessThan <$> decimal)
      <|> char '>' *> (JSArrLenMoreThan <$> decimal)
  void $ char ']'
  pure a

hasKeyP :: (Stream s Identity Char) => Parsec s u JsonObjectQuery
hasKeyP = JSObjHasSub . T.pack <$> many1 anyChar

--  ____       _       _

-- |  _ \ _ __(_)_ __ | |_ ___ _ __
-- | |_) | '__| | '_ \| __/ _ \ '__|
-- |  __/| |  | | | | | ||  __/ |
-- |_|   |_|  |_|_| |_|\__\___|_|
--
-- Print JS queries
renderJSQuery :: JsonQuery -> Text
renderJSQuery = toStrict . toLazyText . renderJsonQuery

renderJsonQuery :: JsonQuery -> Builder
renderJsonQuery (TextQuery q) = "?\"" <> renderJSTextQuery q
renderJsonQuery (NumQuery q) = "?#" <> renderJSNumQuery q
renderJsonQuery (TypeQuery q) = "?:" <> renderJSTypeQuery q
renderJsonQuery (BoolQuery q) = "?=" <> renderJSBoolQuery q
renderJsonQuery (ArrayQuery q) = "?[" <> renderJSArrayQuery q <> "]"
renderJsonQuery (ObjectQuery (JSObjHasSub sub)) = "?" <> fromText sub
renderJsonQuery AnyQuery = "?"
renderJsonQuery (ObjectQuery (JSObjSubQuery sub q)) =
  "|" <> fromText sub <> renderJsonQuery q

renderJSTextQuery :: JsonTextQuery -> Builder
renderJSTextQuery (JSTextLevenshtein obj dist) = fromText obj <> "\"<" <> fromString (show dist)
renderJSTextQuery (JSTextEq obj) = fromText obj <> "\""

renderJSNumQuery :: JsonNumQuery -> Builder
renderJSNumQuery (JSNumEqual x) = "=" <> fromString (show x)
renderJSNumQuery (JSNumLessThan x) = "<" <> fromString (show x)
renderJSNumQuery (JSNumMoreThan x) = ">" <> fromString (show x)
renderJSNumQuery (JSNumWithin x y) =
  "[" <> fromString (show x) <> ":" <> fromString (show y) <> "]"

renderJSTypeQuery :: JsonTypeQuery -> Builder
renderJSTypeQuery JSIsText = "text"
renderJSTypeQuery JSIsNum = "number"
renderJSTypeQuery JSIsObject = "object"
renderJSTypeQuery JSIsArray = "array"
renderJSTypeQuery JSIsBool = "bool"
renderJSTypeQuery JSIsNull = "null"

renderJSBoolQuery :: JsonBoolQuery -> Builder
renderJSBoolQuery JSBoolTrue = "true"
renderJSBoolQuery JSBoolFalse = "false"

renderJSArrayQuery :: JsonArrayQuery -> Builder
renderJSArrayQuery (JSArrLenEq i) = "=" <> fromString (show i)
renderJSArrayQuery (JSArrLenLessThan i) = "<" <> fromString (show i)
renderJSArrayQuery (JSArrLenMoreThan i) = ">" <> fromString (show i)
