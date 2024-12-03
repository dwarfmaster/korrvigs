module Korrvigs.Event.Sync where

import Conduit (throwM)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Value (Array, Null, String))
import Data.Aeson.Decoding (decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Vector as V
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Event.ICalendar
import Korrvigs.Event.SQL
import Korrvigs.FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Metadata
import Korrvigs.Monad
import Linear.V2
import Opaleye hiding (not)
import System.Directory (doesFileExist, getDirectoryContents, removeFile)
import System.FilePath (joinPath, takeExtension)

eventFromRow :: EventRow -> Entry -> Event
eventFromRow erow entry = MkEvent entry (erow ^. sqlEventCalendar) (T.pack $ erow ^. sqlEventFile) (erow ^. sqlEventUID)

dLoadImpl :: (MonadKorrvigs m) => Id -> ((Entry -> Event) -> Entry) -> m (Maybe Entry)
dLoadImpl i cstr = do
  sel <- rSelectOne $ do
    erow <- selectTable eventsTable
    where_ $ erow ^. sqlEventName .== sqlId i
    pure erow
  case (sel :: Maybe EventRow) of
    Nothing -> pure Nothing
    Just erow -> pure . Just . cstr $ eventFromRow erow

dRemoveDBImpl :: Id -> [Delete Int64]
dRemoveDBImpl i =
  [ Delete
      { dTable = eventsTable,
        dWhere = \erow -> erow ^. sqlEventName .== sqlId i,
        dReturning = rCount
      }
  ]

dRemoveImpl :: (MonadKorrvigs m) => Text -> Text -> m ()
dRemoveImpl calendar ics = do
  rt <- eventsDirectory
  let path = joinPath [rt, T.unpack calendar, T.unpack ics]
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path
  conn <- pgSQL
  void $
    liftIO $
      runDelete conn $
        Delete
          { dTable = eventsTable,
            dWhere = \erow -> (erow ^. sqlEventFile) .== sqlStrictText ics .&& (erow ^. sqlEventCalendar) .== sqlStrictText calendar,
            dReturning = rCount
          }

eventsDirectory :: (MonadKorrvigs m) => m FilePath
eventsDirectory = joinPath . (: ["events"]) <$> root

allEvents :: (MonadKorrvigs m) => m [(Text, Text)]
allEvents = do
  rt <- eventsDirectory
  calendars <- liftIO $ getDirectoryContents rt
  events <- forM calendars $ \cal -> do
    let calDir = joinPath [rt, cal]
    events <- liftIO $ getDirectoryContents calDir
    pure $ (T.pack cal,) . T.pack <$> filter (\p -> takeExtension p == ".ics") events
  pure $ concat events

listOne :: (MonadKorrvigs m) => (Text, Text) -> m (Maybe (Id, Text, Text, ICalFile, ICalEvent))
listOne (calendar, ics) = do
  rt <- eventsDirectory
  let path = joinPath [rt, T.unpack calendar, T.unpack ics]
  parsed <- liftIO $ parseICalFile path
  case parsed of
    Left _ -> pure Nothing
    Right ical -> case M.lookup "X-KORRVIGS-NAME" (ical ^. icContent . icValues) of
      Just [val] -> pure $ (MkId $ val ^. icValue,calendar,ics,ical,) <$> ical ^. icEvent
      _ -> pure Nothing

dListImpl :: (MonadKorrvigs m) => m (Set (Id, Text, Text))
dListImpl = do
  evs <- allEvents
  withIds <- mapM listOne evs
  pure $ S.fromList $ map (\(a, b, c, _, _) -> (a, b, c)) $ catMaybes withIds

register :: (MonadKorrvigs m) => (Text, Text) -> m (Id, ICalFile, ICalEvent, Bool)
register (calendar, ics) = do
  rt <- eventsDirectory
  let path = joinPath [rt, T.unpack calendar, T.unpack ics]
  ical <-
    liftIO (parseICalFile path)
      >>= throwEither (\err -> KMiscError $ "Failed to read \"" <> T.pack path <> "\" : " <> err)
  case ical ^. icEvent of
    Nothing -> throwM $ KMiscError $ "ics file \"" <> T.pack path <> "\" has no event"
    Just ievent -> case M.lookup "X-KORRVIGS-NAME" (ievent ^. iceContent . icValues) of
      Just [val] -> pure (MkId $ val ^. icValue, ical, ievent, False)
      _ -> do
        let summary = ievent ^. iceSummary
        let startSpec = ievent ^? iceStart . _Just
        let start = resolveICalTime ical <$> startSpec
        i <- newId $ imk "ics" & idTitle .~ summary & idDate .~ start
        let nevent = ievent & iceContent . icValues . at "X-KORRVIGS-NAME" ?~ [ICValue M.empty (unId i)]
        let ncal = ical & icEvent ?~ nevent
        liftIO $ BSL.writeFile path $ renderICalFile ncal
        pure (i, ncal, nevent, True)

syncOne :: (MonadKorrvigs m) => Id -> Text -> Text -> ICalFile -> ICalEvent -> m RelData
syncOne i calendar ics ifile ical = do
  prev <- load i
  case prev of
    Nothing -> pure ()
    Just prevEntry ->
      case prevEntry ^. kindData of
        EventD event
          | Just (event ^. eventUid) == ical ^? iceUid ->
              dispatchRemoveDB prevEntry
        _ -> dispatchRemove prevEntry
  mtdt <- syncEvent i calendar ics ifile ical
  let extras = mtdtExtras mtdt
  pure $
    RelData
      { _relSubOf = fromMaybe [] $ extras ^. mtdtParents,
        _relRefTo = []
      }

eventMtdt :: ICalFile -> ICalEvent -> Metadata
eventMtdt ical ievent = reifyMetadata extras $ mconcat [flip MValue False <$> basic, categories]
  where
    prepBasic :: (Text, [ICalValue Text]) -> Maybe (Text, Value)
    prepBasic (nm, vs) = case T.stripPrefix "X-KORRMTDT-" nm of
      Nothing -> Nothing
      Just nnm ->
        let jsons :: Maybe [Value] = mapM (decode . LEnc.encodeUtf8 . LT.fromStrict . (^. icValue)) vs
         in case jsons of
              Nothing -> Nothing
              Just [] -> Just (nnm, Null)
              Just [x] -> Just (nnm, x)
              Just xs -> Just (nnm, Array $ V.fromList xs)
    basic :: Map Text Value
    basic = ievent ^. iceContent . icValues . to M.toList . to (map prepBasic) . to catMaybes . to M.fromList
    categories = M.singleton "categories" $ flip MValue True $ Array $ V.fromList $ String <$> ievent ^. iceCategories
    extras :: MtdtExtras
    extras =
      def
        & mtdtGeometry .~ geometry
        & mtdtDate .~ dt
        & mtdtDuration .~ dur
        & mtdtText .~ textContent
        & mtdtTitle .~ title
    textContent :: Maybe Text
    textContent =
      let v = T.intercalate " " $ catMaybes [ievent ^. iceComment, ievent ^. iceSummary, ievent ^. iceDescription]
       in if T.null v then Nothing else Just v
    title :: Maybe Text
    title = ievent ^. iceSummary
    geometry :: Maybe Geometry
    geometry = GeoPoint . uncurry V2 <$> ievent ^. iceGeo
    dt :: Maybe ZonedTime
    dt = resolveICalTime ical <$> ievent ^. iceStart
    dur :: Maybe CalendarDiffTime
    dur = case (ievent ^. iceStart, ievent ^. iceEnd) of
      (Just st, Just nd) ->
        let stTime = resolveICalTime ical st
         in let ndTime = resolveICalTime ical nd
             in let diff = diffUTCTime (zonedTimeToUTC ndTime) (zonedTimeToUTC stTime)
                 in Just $ calendarTimeTime diff
      _ -> calendarTimeTime <$> ievent ^. iceDuration

syncEvent :: (MonadKorrvigs m) => Id -> Text -> Text -> ICalFile -> ICalEvent -> m (Map Text Value)
syncEvent i calendar ics ifile ical = do
  let mtdt' = eventMtdt ifile ical
  let mtdt = view metaValue <$> mtdt'
  let extras = mtdtExtras mtdt
  let geom = extras ^. mtdtGeometry
  let tm = extras ^. mtdtDate
  let dur = extras ^. mtdtDuration
  let erow = EntryRow i Event tm dur geom Nothing :: EntryRow
  let mrows = (\(key, val) -> MetadataRow i key val False) <$> M.toList mtdt :: [MetadataRow]
  let evrow = EventRow i calendar (T.unpack ics) (ical ^. iceUid) :: EventRow
  let txt = ical ^. iceSummary
  atomicSQL $ \conn -> do
    void $
      runInsert conn $
        Insert
          { iTable = entriesTable,
            iRows = [toFields erow],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    void $
      runInsert conn $
        Insert
          { iTable = eventsTable,
            iRows = [toFields evrow],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    void $
      runInsert conn $
        Insert
          { iTable = entriesMetadataTable,
            iRows = toFields <$> mrows,
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    case txt of
      Nothing -> pure ()
      Just t ->
        void $
          runUpdate conn $
            Update
              { uTable = entriesTable,
                uUpdateWith = sqlEntryText .~ toNullable (tsParseEnglish $ sqlStrictText t),
                uWhere = \row -> row ^. sqlEntryName .== sqlId i,
                uReturning = rCount
              }
  pure mtdt

syncOneEvent :: (MonadKorrvigs m) => Id -> Text -> Text -> ICalFile -> ICalEvent -> m RelData
syncOneEvent i calendar ics ifile ievent = do
  mtdt <- syncEvent i calendar ics ifile ievent
  let extras = mtdtExtras mtdt
  pure $
    RelData
      { _relSubOf = fromMaybe [] $ extras ^. mtdtParents,
        _relRefTo = []
      }

dSyncOneImpl :: (MonadKorrvigs m) => Text -> Text -> m RelData
dSyncOneImpl calendar ics =
  listOne (calendar, ics) >>= \case
    Nothing -> pure $ RelData [] []
    Just (i, _, _, ifile, ievent) -> syncOneEvent i calendar ics ifile ievent

dSyncImpl :: (MonadKorrvigs m) => m (Map Id RelData)
dSyncImpl = do
  files <- allEvents
  evs <- catMaybes <$> forM files listOne
  rdata <- forM evs $ \(i, calendar, ics, ifile, ievent) -> (i,) <$> syncOneEvent i calendar ics ifile ievent
  pure $ M.fromList rdata
