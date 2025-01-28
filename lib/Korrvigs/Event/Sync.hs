module Korrvigs.Event.Sync where

import Conduit (throwM)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Result (Error, Success), Value, fromJSON, toJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime
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
    Right ical -> case ical ^? icEvent . _Just . iceId . _Just of
      Just i -> pure $ (i,calendar,ics,ical,) <$> ical ^. icEvent
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
    Just ievent -> case ievent ^. iceId of
      Just i -> pure (i, ical, ievent, False)
      _ -> do
        let summary = ievent ^. iceSummary
        let startSpec = ievent ^? iceStart . _Just
        let start = resolveICalTime ical <$> startSpec
        i <- newId $ imk "ics" & idTitle .~ summary & idDate .~ start & idLanguage ?~ "fr"
        let nevent = ievent & iceId ?~ i & iceMtdt . at (mtdtName Title) ?~ toJSON summary
        let ncal = ical & icEvent ?~ nevent
        liftIO $ BSL.writeFile path $ renderICalFile ncal
        pure (i, ncal, nevent, True)

syncEvent :: (MonadKorrvigs m) => Id -> Text -> Text -> ICalFile -> ICalEvent -> m ()
syncEvent i calendar ics ifile ical = do
  let mtdt = ical ^. iceMtdt
  let geom = ical ^. iceGeometry
  let tm = resolveICalTime ifile <$> ical ^. iceStart
  let dur = case (tm, ical ^. iceEnd) of
        (Just st, Just nd) ->
          let ndTime = resolveICalTime ifile nd
           in let diff = diffUTCTime (zonedTimeToUTC ndTime) (zonedTimeToUTC st)
               in Just $ calendarTimeTime diff
        _ -> calendarTimeTime <$> ical ^. iceDuration
  let erow = EntryRow i Event tm dur geom Nothing :: EntryRow
  let mrows = uncurry (MetadataRow i) <$> M.toList mtdt :: [MetadataRow]
  let evrow = EventRow i calendar (T.unpack ics) (ical ^. iceUid) :: EventRow
  let txt = T.intercalate " " $ catMaybes [ical ^. iceComment, ical ^. iceSummary, ical ^. iceDescription]
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
    unless (T.null txt) $
      void $
        runUpdate conn $
          Update
            { uTable = entriesTable,
              uUpdateWith = sqlEntryText .~ toNullable (tsParseEnglish $ sqlStrictText txt),
              uWhere = \row -> row ^. sqlEntryName .== sqlId i,
              uReturning = rCount
            }

syncOneEvent :: (MonadKorrvigs m) => Id -> Text -> Text -> ICalFile -> ICalEvent -> m RelData
syncOneEvent i calendar ics ifile ievent = do
  prev <- load i
  forM_ prev dispatchRemoveDB
  syncEvent i calendar ics ifile ievent
  pure $
    RelData
      { _relSubOf = ievent ^. iceParents,
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

dUpdateMetadataImpl :: (MonadKorrvigs m) => Event -> Map Text Value -> [Text] -> m ()
dUpdateMetadataImpl event upd rm = do
  let cal = event ^. eventCalendar
  let ics = event ^. eventFile
  rt <- eventsDirectory
  let path = joinPath [rt, T.unpack cal, T.unpack ics]
  ical <-
    liftIO (parseICalFile path)
      >>= throwEither (\err -> KMiscError $ "Failed to read \"" <> T.pack path <> "\" : " <> err)
  let ncal = ical & icEvent . _Just %~ doMtdt
  liftIO $ BSL.writeFile path $ renderICalFile ncal
  where
    rmMtdt :: Text -> ICalEvent -> ICalEvent
    rmMtdt "categories" ievent = ievent & iceCategories .~ []
    rmMtdt key ievent = ievent & iceMtdt %~ M.delete (CI.mk key)
    updMtdt :: Text -> Value -> ICalEvent -> ICalEvent
    updMtdt "categories" val ievent = case fromJSON val of
      Success cats -> ievent & iceCategories .~ cats
      Error _ -> ievent
    updMtdt key val ievent = ievent & iceMtdt . at (CI.mk key) ?~ val
    doMtdt :: ICalEvent -> ICalEvent
    doMtdt = foldr (.) id $ (uncurry updMtdt <$> M.toList upd) ++ (rmMtdt <$> rm)
