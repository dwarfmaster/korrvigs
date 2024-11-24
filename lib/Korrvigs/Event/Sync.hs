module Korrvigs.Event.Sync where

import Conduit (throwM)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Value (Array, Null))
import Data.Aeson.Decoding (decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import qualified Data.Vector as V
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Event.ICalendar
import Korrvigs.Event.SQL
import Korrvigs.FTS
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Metadata
import Korrvigs.Monad
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
            dWhere = \erow -> erow ^. sqlEventFile .== sqlString path,
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
    pure $ (T.pack calDir,) . T.pack <$> filter (\p -> takeExtension p == ".ics") events
  pure $ concat events

listOne :: (MonadKorrvigs m) => (Text, Text) -> m (Maybe (Id, Text, Text, ICalEvent))
listOne (calendar, ics) = do
  rt <- eventsDirectory
  let path = joinPath [rt, T.unpack calendar, T.unpack ics]
  parsed <- liftIO $ parseICalFile path
  case parsed of
    Left _ -> pure Nothing
    Right ical -> case M.lookup "X-KORRVIGS-NAME" (ical ^. icContent . icValues) of
      Just [val] -> pure $ (MkId $ val ^. icValue,calendar,ics,) <$> ical ^. icEvent
      _ -> pure Nothing

dListImpl :: (MonadKorrvigs m) => m (Set (Id, Text, Text))
dListImpl = do
  evs <- allEvents
  withIds <- mapM listOne evs
  pure $ S.fromList $ map (\(a, b, c, _) -> (a, b, c)) $ catMaybes withIds

register :: (MonadKorrvigs m) => (Text, Text) -> m (Id, ICalEvent)
register (calendar, ics) = do
  rt <- eventsDirectory
  let path = joinPath [rt, T.unpack calendar, T.unpack ics]
  ical <-
    liftIO (parseICalFile path)
      >>= throwEither (\err -> KMiscError $ "Failed to read \"" <> T.pack path <> "\" : " <> err)
  case ical ^. icEvent of
    Nothing -> throwM $ KMiscError $ "ics file \"" <> T.pack path <> "\" has no event"
    Just ievent -> case M.lookup "X-KORRVIGS-NAME" (ievent ^. iceContent . icValues) of
      Just [val] -> pure (MkId $ val ^. icValue, ievent)
      _ -> do
        let summary = ievent ^? iceContent . icValues . at "SUMMARY" . _Just . _Cons . _1 . icValue
        let startSpec = ievent ^? iceStart . _Just
        let start = resolveICalTime ical <$> startSpec
        i <- newId $ imk "ics" & idTitle .~ summary & idDate .~ start
        let nevent = ievent & iceContent . icValues . at "X-KORRVIGS-NAME" ?~ [ICValue M.empty (unId i)]
        let ncal = ical & icEvent ?~ nevent
        liftIO $ BSL.writeFile path $ renderICalFile ncal
        pure (i, nevent)

syncOne :: (MonadKorrvigs m) => Id -> Text -> Text -> ICalEvent -> m RelData
syncOne i calendar ics ical = do
  prev <- load i
  case prev of
    Nothing -> pure ()
    Just prevEntry ->
      case prevEntry ^. kindData of
        EventD event
          | Just (event ^. eventUid) == ical ^? iceUid ->
              dispatchRemoveDB prevEntry
        _ -> dispatchRemove prevEntry
  mtdt <- syncEvent i calendar ics ical
  let extras = mtdtExtras mtdt
  pure $
    RelData
      { _relSubOf = fromMaybe [] $ extras ^. mtdtParents,
        _relRefTo = []
      }

-- TODO add specific ical metadata
eventMtdt :: ICalEvent -> Map Text Value
eventMtdt ievent = basic
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

syncEvent :: (MonadKorrvigs m) => Id -> Text -> Text -> ICalEvent -> m (Map Text Value)
syncEvent i calendar ics ical = do
  let mtdt = eventMtdt ical
  let extras = mtdtExtras mtdt
  let geom = extras ^. mtdtGeometry
  let tm = extras ^. mtdtDate
  let dur = extras ^. mtdtDuration
  let erow = EntryRow i Event tm dur geom Nothing :: EntryRow
  let mrows = (\(key, val) -> MetadataRow i key val False) <$> M.toList mtdt :: [MetadataRow]
  let evrow = EventRow i calendar (T.unpack ics) (ical ^. iceUid) :: EventRow
  let txt = ical ^? iceContent . icValues . at "SUMMARY" . _Just . _Cons . _1 . icValue
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

syncOneEvent :: (MonadKorrvigs m) => Id -> Text -> Text -> ICalEvent -> m RelData
syncOneEvent i calendar ics ievent = do
  mtdt <- syncEvent i calendar ics ievent
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
    Just (i, _, _, ievent) -> syncOneEvent i calendar ics ievent

dSyncImpl :: (MonadKorrvigs m) => m (Map Id RelData)
dSyncImpl = do
  files <- allEvents
  evs <- catMaybes <$> forM files listOne
  rdata <- forM evs $ \(i, calendar, ics, ievent) -> (i,) <$> syncOneEvent i calendar ics ievent
  pure $ M.fromList rdata
