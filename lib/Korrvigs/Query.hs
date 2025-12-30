{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Query where

import Control.Lens hiding (noneOf, (.=), (.>))
import Control.Monad
import Data.Aeson
import Data.Default
import Data.Foldable
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
import Korrvigs.File.SQL
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Note.SQL
import Korrvigs.Syndicate.SQL
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye
import Opaleye hiding (null)
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
  | ByTitle
  deriving (Eq, Show)

instance Default SortCriterion where
  def = ById

data SortOrder
  = SortAsc
  | SortDesc
  deriving (Eq, Show)

instance Default SortOrder where
  def = SortAsc

data NoteQuery = NoteQuery deriving (Show, Eq)

newtype FileQuery = FileQuery
  { _queryFileMime :: Maybe Text -- PSQL regexp for mime
  }
  deriving (Show, Eq)

data EventQuery = EventQuery deriving (Show, Eq)

data CalendarQuery = CalendarQuery deriving (Show, Eq)

newtype SyndicateQuery = SyndicateQuery
  { _querySyndicateUrl :: Maybe Text -- PSQL regexp for url
  }
  deriving (Show, Eq)

data KindQuery
  = KindQueryNote NoteQuery
  | KindQueryFile FileQuery
  | KindQueryEvent EventQuery
  | KindQueryCalendar CalendarQuery
  | KindQuerySyndicate SyndicateQuery
  deriving (Show, Eq)

data QueryRel = QueryRel
  { _relOther :: Query,
    _relRec :: Bool
  }
  deriving (Show)

data QueryInCollection = QueryInCol
  { _colName :: Text,
    _colEntry :: Id
  }
  deriving (Show)

data Query = Query
  { _queryId :: [Id],
    _queryTitle :: Maybe Text, -- PSQL regexp as described here: https://www.postgresql.org/docs/current/functions-matching.html#FUNCTIONS-POSIX-REGEXP
    _queryText :: Maybe FTS.Query,
    _queryBefore :: Maybe ZonedTime,
    _queryAfter :: Maybe ZonedTime,
    _queryGeo :: Maybe Polygon,
    _queryDist :: Maybe (Point, Double),
    _queryKind :: Maybe KindQuery,
    _queryMtdt :: [(Text, JsonQuery)],
    _queryInCollection :: Maybe QueryInCollection,
    _querySubOf :: Maybe QueryRel,
    _queryParentOf :: Maybe QueryRel,
    _queryMentioning :: Maybe QueryRel,
    _queryMentionedBy :: Maybe QueryRel,
    _queryShowHidden :: Bool,
    _querySort :: (SortCriterion, SortOrder),
    _queryMaxResults :: Maybe Int
  }
  deriving (Show)

instance Default Query where
  def = Query def def def def def def def def def def def def def def False def def

makeLenses ''Query
makeLenses ''QueryRel
makeLenses ''QueryInCollection
makeLenses ''NoteQuery
makeLenses ''FileQuery
makeLenses ''EventQuery
makeLenses ''CalendarQuery
makeLenses ''SyndicateQuery
makePrisms ''KindQuery

instance ToJSON JsonQuery where
  toJSON = toJSON . renderJSQuery

instance FromJSON JsonQuery where
  parseJSON = withText "JsonQuery" $ \txt -> case parseJSQuery txt of
    Left err -> fail $ T.unpack err
    Right jsq -> pure jsq

instance ToJSON QueryRel where
  toJSON rel =
    object
      [ "query" .= (rel ^. relOther),
        "rec" .= (rel ^. relRec)
      ]

instance FromJSON QueryRel where
  parseJSON = withObject "QueryRel" $ \obj ->
    QueryRel
      <$> obj .: "query"
      <*> obj .: "rec"

instance ToJSON SortCriterion where
  toJSON ByDate = object ["criterion" .= ("date" :: Text)]
  toJSON (ByTSRank q) = object ["criterion" .= ("tsrank" :: Text), "query" .= q]
  toJSON (ByDistanceTo pt) = object ["criterion" .= ("distance" :: Text), "point" .= pt]
  toJSON ById = object ["criterion" .= ("id" :: Text)]
  toJSON ByTitle = object ["criterion" .= ("title" :: Text)]

instance FromJSON SortCriterion where
  parseJSON = withObject "SortCriterion" $ \obj ->
    (obj .: "criterion") >>= \case
      "date" -> pure ByDate
      "tsrank" -> ByTSRank <$> obj .: "query"
      "distance" -> ByDistanceTo <$> obj .: "point"
      "id" -> pure ById
      "title" -> pure ByTitle
      s -> fail $ s <> " is not a valid sort criterion"

instance ToJSON SortOrder where
  toJSON SortAsc = "ascending"
  toJSON SortDesc = "descending"

instance FromJSON SortOrder where
  parseJSON = withText "SortOrder" $ \case
    "ascending" -> pure SortAsc
    "descending" -> pure SortDesc
    s -> fail $ T.unpack s <> " is not a valid sort order"

instance ToJSON NoteQuery where
  toJSON NoteQuery = object []

instance FromJSON NoteQuery where
  parseJSON = withObject "NoteQuery" $ const $ pure NoteQuery

instance Default NoteQuery where
  def = NoteQuery

instance ToJSON FileQuery where
  toJSON (FileQuery mime) = object ["mime" .= mimeRegex | mimeRegex <- toList mime]

instance FromJSON FileQuery where
  parseJSON = withObject "FileQuery" $ \obj ->
    FileQuery <$> obj .:? "mime"

instance Default FileQuery where
  def = FileQuery def

instance ToJSON EventQuery where
  toJSON EventQuery = object []

instance FromJSON EventQuery where
  parseJSON = withObject "EventQuery" $ const $ pure EventQuery

instance Default EventQuery where
  def = EventQuery

instance ToJSON CalendarQuery where
  toJSON CalendarQuery = object []

instance FromJSON CalendarQuery where
  parseJSON = withObject "CalendarQuery" $ const $ pure CalendarQuery

instance Default CalendarQuery where
  def = CalendarQuery

instance ToJSON SyndicateQuery where
  toJSON (SyndicateQuery url) = object ["url" .= urlRegex | urlRegex <- toList url]

instance FromJSON SyndicateQuery where
  parseJSON = withObject "SyndicateQuery" $ \obj ->
    SyndicateQuery <$> obj .:? "url"

instance Default SyndicateQuery where
  def = SyndicateQuery def

instance ToJSON KindQuery where
  toJSON (KindQueryNote nq) = object ["kind" .= Note, "query" .= nq]
  toJSON (KindQueryFile fq) = object ["kind" .= File, "query" .= fq]
  toJSON (KindQueryEvent eq) = object ["kind" .= Event, "query" .= eq]
  toJSON (KindQueryCalendar cq) = object ["kind" .= Calendar, "query" .= cq]
  toJSON (KindQuerySyndicate sq) = object ["kind" .= Syndicate, "query" .= sq]

queryFromKind :: Kind -> KindQuery
queryFromKind = \case
  Note -> KindQueryNote def
  File -> KindQueryFile def
  Event -> KindQueryEvent def
  Calendar -> KindQueryCalendar def
  Syndicate -> KindQuerySyndicate def

queryToKind :: KindQuery -> Kind
queryToKind (KindQueryNote _) = Note
queryToKind (KindQueryFile _) = File
queryToKind (KindQueryEvent _) = Event
queryToKind (KindQueryCalendar _) = Calendar
queryToKind (KindQuerySyndicate _) = Syndicate

instance FromJSON KindQuery where
  parseJSON (String txt) = queryFromKind <$> parseJSON (String txt)
  parseJSON v =
    withObject
      "KindQuery"
      ( \obj ->
          obj .: "kind" >>= \case
            Note -> KindQueryNote <$> obj .: "query"
            File -> KindQueryFile <$> obj .: "query"
            Event -> KindQueryEvent <$> obj .: "query"
            Calendar -> KindQueryCalendar <$> obj .: "query"
            Syndicate -> KindQuerySyndicate <$> obj .: "query"
      )
      v

instance ToJSON QueryInCollection where
  toJSON incol =
    object
      [ "name" .= (incol ^. colName),
        "entry" .= unId (incol ^. colEntry)
      ]

instance FromJSON QueryInCollection where
  parseJSON = withObject "QueryInCollection" $ \obj ->
    QueryInCol
      <$> obj .: "name"
      <*> (MkId <$> obj .: "entry")

instance ToJSON Query where
  toJSON q =
    object $
      [ "id" .= (unId <$> q ^. queryId),
        "mtdt" .= (q ^. queryMtdt),
        "sort" .= (q ^. querySort),
        "hidden" .= (q ^. queryShowHidden)
      ]
        ++ optKP "title" (q ^. queryTitle)
        ++ optKP "text" (q ^. queryText)
        ++ optKP "before" (q ^. queryBefore)
        ++ optKP "after" (q ^. queryAfter)
        ++ optKP "geo" (q ^. queryGeo)
        ++ optKP "distance" (q ^. queryDist)
        ++ optKP "kind" (q ^. queryKind)
        ++ optKP "collection" (q ^. queryInCollection)
        ++ optKP "subof" (q ^. querySubOf)
        ++ optKP "parentof" (q ^. queryParentOf)
        ++ optKP "mentioning" (q ^. queryMentioning)
        ++ optKP "mentionedby" (q ^. queryMentionedBy)
        ++ optKP "maxresults" (q ^. queryMaxResults)

instance FromJSON Query where
  parseJSON = withObject "Query" $ \obj ->
    Query . fmap MkId
      <$> obj .: "id"
      <*> obj .:? "title"
      <*> obj .:? "text"
      <*> obj .:? "before"
      <*> obj .:? "after"
      <*> obj .:? "geo"
      <*> obj .:? "distance"
      <*> obj .:? "kind"
      <*> obj .: "mtdt"
      <*> obj .:? "collection"
      <*> obj .:? "subof"
      <*> obj .:? "parentof"
      <*> obj .:? "mentioning"
      <*> obj .:? "mentionedby"
      <*> obj .: "hidden"
      <*> obj .: "sort"
      <*> obj .:? "maxresults"

compileNoteQuery :: EntryRowSQLR -> NoteQuery -> Select ()
compileNoteQuery entry NoteQuery = where_ $ entry ^. sqlEntryKind .== sqlKind Note

compileFileQuery :: EntryRowSQLR -> FileQuery -> Select ()
compileFileQuery entry fq = case fq ^. queryFileMime of
  Nothing -> where_ $ entry ^. sqlEntryKind .== sqlKind File
  Just mime -> do
    file <- selectTable filesTable
    where_ $ file ^. sqlFileId .== (entry ^. sqlEntryId)
    where_ $ sqlMatchRegexCaseInsensitive (file ^. sqlFileMime) (sqlStrictText mime)

compileEventQuery :: EntryRowSQLR -> EventQuery -> Select ()
compileEventQuery entry EventQuery = where_ $ entry ^. sqlEntryKind .== sqlKind Event

compileCalendarQuery :: EntryRowSQLR -> CalendarQuery -> Select ()
compileCalendarQuery entry CalendarQuery =
  where_ $ entry ^. sqlEntryKind .== sqlKind Calendar

compileSyndicateQuery :: EntryRowSQLR -> SyndicateQuery -> Select ()
compileSyndicateQuery entry (SyndicateQuery Nothing) =
  where_ $ entry ^. sqlEntryKind .== sqlKind Syndicate
compileSyndicateQuery entry sq = do
  syn <- selectTable syndicatesTable
  where_ $ syn ^. sqlSynId .== (entry ^. sqlEntryId)
  forM_ (sq ^. querySyndicateUrl) $ \url ->
    let sqlUrl = fromNullable (sqlStrictText "") (syn ^. sqlSynUrl)
     in where_ $ sqlMatchRegexCaseInsensitive sqlUrl (sqlStrictText url)

-- Match all that are related by the relation table to the result of the query
compileRel :: EntryRowSQLR -> Table a RelRowSQL -> Bool -> QueryRel -> Select ()
compileRel entry tbl direct q = limit 1 $ do
  otherEntry <- compileQuery $ q ^. relOther
  candidate <- closure (selectTable tbl) src tgt $ otherEntry ^. sqlEntryId
  where_ $ entry ^. sqlEntryId .== candidate
  where
    src = if direct then view source else view target
    tgt = if direct then view target else view source
    closure = if q ^. relRec then transitiveClosure else transitiveClosureStep

-- Returns the IDs of the matched entries
compileQuery :: Query -> Select EntryRowSQLR
compileQuery query = do
  entry <- selectTable entriesTable
  -- Id
  unless (null $ query ^. queryId) $
    where_ $
      foldr1 (.||) $
        fmap (\i -> sqlId i .== entry ^. sqlEntryName) $
          query ^. queryId
  -- Title search (case-insensitive regex match)
  forM_ (query ^. queryTitle) $ \regex ->
    where_ $ matchNullable (sqlBool False) (`sqlMatchRegexCaseInsensitive` sqlStrictText regex) $ entry ^. sqlEntryTitle
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
     in where_ $ flip (matchNullable $ sqlBool False) (entry ^. sqlEntryDate) $ \date ->
          matchNullable
            (sqlDt .< date)
            (\dur -> sqlDt .< addInterval date dur)
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
  forM_ (query ^. queryKind) $ \case
    KindQueryNote nq -> compileNoteQuery entry nq
    KindQueryFile fq -> compileFileQuery entry fq
    KindQueryEvent eq -> compileEventQuery entry eq
    KindQueryCalendar cq -> compileCalendarQuery entry cq
    KindQuerySyndicate sq -> compileSyndicateQuery entry sq
  -- Checks againts metadata
  forM_ (query ^. queryMtdt) $ \q -> do
    mtdt <- selectTable entriesMetadataTable
    where_ $ (mtdt ^. sqlEntry) .== (entry ^. sqlEntryId)
    where_ $ mtdt ^. sqlKey .== sqlStrictText (q ^. _1)
    where_ $ compileJsonQuery (q ^. _2) (toNullable $ mtdt ^. sqlValue)
  -- Collection
  forM_ (query ^. queryInCollection) $ \incol -> do
    col <- selectTable notesCollectionsTable
    sqlI <- fromName pure $ sqlId $ incol ^. colEntry
    where_ $ col ^. sqlNoteColId .== sqlI
    where_ $ col ^. sqlNoteColName .== sqlStrictText (incol ^. colName)
    where_ $ col ^. sqlNoteColEntry .== (entry ^. sqlEntryName)
  -- Relations
  forM_ (query ^. querySubOf) $ compileRel entry entriesSubTable False
  forM_ (query ^. queryParentOf) $ compileRel entry entriesSubTable True
  forM_ (query ^. queryMentioning) $ compileRel entry entriesRefTable False
  forM_ (query ^. queryMentionedBy) $ compileRel entry entriesRefTable True
  -- Hide hidden by default
  unless (query ^. queryShowHidden) $ do
    hidden <- selectTextMtdt Hidden $ entry ^. sqlEntryId
    where_ $ isNull hidden
  pure entry

compile :: Query -> (EntryRowSQLR -> Select a) -> Select (EntryRowSQLR, a)
compile query other = lmt (query ^. queryMaxResults) $ sort (query ^. querySort) $ do
  entry <- compileQuery query
  o <- other entry
  pure (entry, o)
  where
    dir :: (SqlOrd b) => SortOrder -> (a -> Field b) -> Order a
    dir SortAsc = asc
    dir SortDesc = desc
    dirNullsLast :: (SqlOrd b) => SortOrder -> (a -> Field_ n b) -> Order a
    dirNullsLast SortAsc = ascNullsLast
    dirNullsLast SortDesc = descNullsLast
    largeTime :: ZonedTime
    largeTime = ZonedTime (LocalTime (fromOrdinalDate 9999 1) (TimeOfDay 0 0 0)) utc
    sort :: (SortCriterion, SortOrder) -> Select (EntryRowSQLR, a) -> Select (EntryRowSQLR, a)
    sort (ById, ord) = orderBy $ dir ord $ \e -> e ^. _1 . sqlEntryName
    sort (ByTSRank q, ord) =
      orderBy $ dir ord $ \e ->
        matchNullable
          (sqlDouble infinity)
          (FTS.tsRank (FTS.sqlQuery q))
          (e ^. _1 . sqlEntryText)
    sort (ByDistanceTo pt, ord) =
      orderBy $ dir ord $ \e ->
        matchNullable
          (sqlDouble infinity)
          (\g -> stDistance (sqlPoint pt) g (sqlBool True))
          (e ^. _1 . sqlEntryGeo)
    sort (ByDate, ord) =
      orderBy $ dir ord $ \e ->
        fromNullable
          (sqlZonedTime largeTime)
          (e ^. _1 . sqlEntryDate)
    sort (ByTitle, ord) =
      orderBy $ dirNullsLast ord $ view $ _1 . sqlEntryTitle
    infinity :: Double
    infinity = 1 / 0
    lmt :: Maybe Int -> Select a -> Select a
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
  (,) . T.pack
    <$> many1 (noneOf "|?")
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
