{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Log where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Maybe
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Utils.JSON (fromJSONM)
import Korrvigs.Utils.Opaleye (makeSqlMapper)
import Opaleye
import Opaleye.Experimental.Enum

data LogEventData
  = MiscEvent Text
  | LoadEvent Id
  | ParseErrorEvent {_logParser :: Text, _logSource :: Text, _logError :: Text}
  | NewEntryEvent Kind Id
  | EntryAlreadyExistsEvent Kind Id
  | MissingCredentialEvent Text
  deriving (Eq, Show, Ord)

instance ToJSON LogEventData where
  toJSON (MiscEvent txt) = String txt
  toJSON (LoadEvent i) = object ["type" .= ("load" :: Text), "entry" .= unId i]
  toJSON (ParseErrorEvent parser src err) =
    object
      [ "type" .= ("parse-error" :: Text),
        "parser" .= parser,
        "source" .= src,
        "error" .= err
      ]
  toJSON (NewEntryEvent kd i) =
    object
      [ "type" .= ("new-entry" :: Text),
        "kind" .= kd,
        "id" .= i
      ]
  toJSON (EntryAlreadyExistsEvent kd i) =
    object
      [ "type" .= ("entry-already-exists" :: Text),
        "kind" .= kd,
        "id" .= i
      ]
  toJSON (MissingCredentialEvent for) =
    object
      [ "type" .= ("missing-credential" :: Text),
        "for" .= for
      ]

instance FromJSON LogEventData where
  parseJSON (String txt) = pure $ MiscEvent txt
  parseJSON (Object obj) = do
    kd :: Text <- obj .: "type"
    case kd of
      "load" -> LoadEvent . MkId <$> obj .: "entry"
      "parse-error" ->
        ParseErrorEvent <$> obj .: "parser" <*> obj .: "source" <*> obj .: "error"
      "new-entry" ->
        NewEntryEvent <$> obj .: "kind" <*> obj .: "id"
      "entry-already-exists" ->
        EntryAlreadyExistsEvent <$> obj .: "kind" <*> obj .: "id"
      "missing-credential" ->
        MissingCredentialEvent <$> obj .: "for"
      _ -> fail $ T.unpack $ "Unknown log error kind " <> kd
  parseJSON _ = fail $ "Expected string or object for LogEventData"

data LogLevel
  = LogTrace
  | LogInfo
  | LogWarning
  | LogError
  deriving (Eq, Show, Ord, Enum, Bounded)

instance ToJSON LogLevel where
  toJSON LogTrace = String "trace"
  toJSON LogInfo = String "info"
  toJSON LogWarning = String "warning"
  toJSON LogError = String "error"

data LogEvent = LogEvent
  { _logTime :: UTCTime,
    _logModule :: Text,
    _logLine :: Int,
    _logLevel :: LogLevel,
    _logData :: LogEventData,
    _logChilds :: [LogEvent]
  }
  deriving (Eq, Show, Ord)

makeLenses ''LogEventData
makePrisms ''LogEventData
makeLenses ''LogEvent

instance ToJSON LogEvent where
  toJSON ev =
    object
      [ "time" .= (ev ^. logTime),
        "module" .= (ev ^. logModule),
        "line" .= (ev ^. logLine),
        "level" .= (ev ^. logLevel),
        "data" .= (ev ^. logData),
        "childs" .= (ev ^. logChilds)
      ]

data SqlLogLevel

toSqlLogLevel :: LogLevel -> String
toSqlLogLevel LogTrace = "trace"
toSqlLogLevel LogInfo = "info"
toSqlLogLevel LogWarning = "warning"
toSqlLogLevel LogError = "error"

sqlLogLevelMapper :: EnumMapper SqlLogLevel LogLevel
sqlLogLevelMapper = makeSqlMapper "loglevel" toSqlLogLevel

instance DefaultFromField SqlLogLevel LogLevel where
  defaultFromField = enumFromField sqlLogLevelMapper

instance Default ToFields LogLevel (Field SqlLogLevel) where
  def = enumToFields sqlLogLevelMapper

instance IsSqlType SqlLogLevel where
  showSqlType _ = "loglevel"

data LogEventRowImpl a b c d e f g = LogEventRow
  { _evsqlId :: a,
    _evsqlParent :: b,
    _evsqlTime :: c,
    _evsqlModule :: d,
    _evsqlLine :: e,
    _evsqlLevel :: f,
    _evsqlPayload :: g
  }

makeLenses ''LogEventRowImpl
$(makeAdaptorAndInstanceInferrable "pLogEventRow" ''LogEventRowImpl)

type LogEventRowW = LogEventRowImpl (Maybe Int) (Maybe Int) UTCTime Text Int LogLevel Value

type LogEventRowR = LogEventRowImpl Int (Maybe Int) UTCTime Text Int LogLevel Value

type LogEventSQLW = LogEventRowImpl (Maybe (Field SqlInt4)) (FieldNullable SqlInt4) (Field SqlTimestamptz) (Field SqlText) (Field SqlInt4) (Field SqlLogLevel) (Field SqlJsonb)

type LogEventSQLR = LogEventRowImpl (Field SqlInt4) (FieldNullable SqlInt4) (Field SqlTimestamptz) (Field SqlText) (Field SqlInt4) (Field SqlLogLevel) (Field SqlJsonb)

instance Default ToFields LogEventRowW LogEventSQLW where
  def = pLogEventRow $ LogEventRow def def def def def def def

logEventTable :: Table LogEventSQLW LogEventSQLR
logEventTable =
  table "log_events" $
    pLogEventRow $
      LogEventRow
        (optionalTableField "id")
        (tableField "parent")
        (tableField "time")
        (tableField "module")
        (tableField "line")
        (tableField "level")
        (tableField "payload")

loadEvents :: Connection -> [LogEventRowR] -> IO [LogEvent]
loadEvents conn evs = do
  catMaybes <$> mapM (recLoadEvent conn) evs

recLoadEvent :: Connection -> LogEventRowR -> IO (Maybe LogEvent)
recLoadEvent conn row = do
  childsSQL <- runSelect conn $ do
    e <- selectTable logEventTable
    where_ $ matchNullable (sqlBool False) (.== sqlInt4 (row ^. evsqlId)) $ e ^. evsqlParent
    pure e
  childs <- mapM (recLoadEvent conn) childsSQL
  case fromJSONM (row ^. evsqlPayload) of
    Nothing -> pure Nothing
    Just dat ->
      pure $
        Just $
          LogEvent
            { _logTime = row ^. evsqlTime,
              _logModule = row ^. evsqlModule,
              _logLine = row ^. evsqlLine,
              _logLevel = row ^. evsqlLevel,
              _logData = dat,
              _logChilds = catMaybes childs
            }

logEvent :: Connection -> Maybe Int -> Text -> Int -> LogLevel -> LogEventData -> IO Int
logEvent conn parent md line lvl dat = do
  time <- getCurrentTime
  let row :: LogEventRowW = LogEventRow Nothing parent time md line lvl (toJSON dat)
  ids :: [Int] <-
    runInsert conn $
      Insert
        { iTable = logEventTable,
          iRows = [toFields row],
          iReturning = rReturning $ \r -> r ^. evsqlId,
          iOnConflict = Just doNothing
        }
  case ids of
    [i] -> pure i
    _ -> error "Log insertion failed to return anything"
