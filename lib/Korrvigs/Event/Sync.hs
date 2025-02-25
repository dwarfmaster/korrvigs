module Korrvigs.Event.Sync where

import Conduit (throwM)
import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Result (Error, Success), Value, fromJSON, toJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Default
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
import Korrvigs.Utils.DateTree
import Opaleye hiding (not)
import System.Directory (doesFileExist, removeFile)
import System.FilePath (joinPath, takeBaseName)

eventTreeType :: DateTreeType
eventTreeType = def & dtYear .~ True & dtMonth .~ True

eventIdFromPath :: FilePath -> (Id, Id)
eventIdFromPath path = MkId *** (MkId . T.drop 1) $ T.breakOn "_" $ T.pack $ takeBaseName path

eventFromRow :: EventRow -> Entry -> Event
eventFromRow erow entry = MkEvent entry (erow ^. sqlEventCalendar) (erow ^. sqlEventFile) (erow ^. sqlEventUID)

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

dRemoveImpl :: (MonadKorrvigs m) => FilePath -> m ()
dRemoveImpl path = do
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path

eventsDirectory :: (MonadKorrvigs m) => m FilePath
eventsDirectory = joinPath . (: ["events"]) <$> root

allEvents :: (MonadKorrvigs m) => m [FilePath]
allEvents = do
  rt <- eventsDirectory
  let dtt = eventTreeType
  files <- listFiles rt dtt
  pure $ view _1 <$> files

dListImpl :: (MonadKorrvigs m) => m (Set FilePath)
dListImpl = S.fromList <$> allEvents

createIdFor :: (MonadKorrvigs m) => ICalFile -> ICalEvent -> m Id
createIdFor ical ievent = do
  let language = extractMtdt Language $ ievent ^. iceMtdt
  let title = extractMtdt Title $ ievent ^. iceMtdt
  let summary = ievent ^. iceSummary
  let startSpec = ievent ^? iceStart . _Just
  let start = resolveICalTime ical <$> startSpec
  let parents = ievent ^. iceParents
  newId $
    imk "ics"
      & idTitle .~ (title <|> summary)
      & idDate .~ start
      & idLanguage ?~ fromMaybe "fr" language
      & idParent .~ listToMaybe parents

register :: (MonadKorrvigs m) => ICalFile -> m Id
register ical =
  case ical ^. icEvent of
    Nothing -> throwM $ KMiscError "ics has no event"
    Just ievent -> do
      let summary = ievent ^. iceSummary
      let nevent' = ievent & iceMtdt . at (mtdtName Title) ?~ toJSON summary
      createIdFor ical nevent'

syncEvent :: (MonadKorrvigs m) => Id -> Id -> FilePath -> ICalFile -> ICalEvent -> m ()
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
  let evrow = EventRow i calendar ics (ical ^. iceUid) :: EventRow
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

syncOneEvent :: (MonadKorrvigs m) => Id -> Id -> FilePath -> ICalFile -> ICalEvent -> m RelData
syncOneEvent i calendar ics ifile ievent = do
  prev <- load i
  forM_ prev dispatchRemoveDB
  syncEvent i calendar ics ifile ievent
  pure $
    RelData
      { _relSubOf = calendar : ievent ^. iceParents,
        _relRefTo = []
      }

dSyncOneImpl :: (MonadKorrvigs m) => FilePath -> m RelData
dSyncOneImpl path = do
  let (i, calendar) = eventIdFromPath path
  liftIO (parseICalFile path) >>= \case
    Left err -> throwM $ KMiscError $ "Failed to parse \"" <> T.pack path <> "\": " <> err
    Right ifile -> case ifile ^. icEvent of
      Nothing -> throwM $ KMiscError $ "Ics file \"" <> T.pack path <> "\" has no VEVENT"
      Just ievent -> syncOneEvent i calendar path ifile ievent

dSyncImpl :: (MonadKorrvigs m) => m (Map Id RelData)
dSyncImpl = do
  files <- allEvents
  rdata <- forM files $ \path -> do
    let i = eventIdFromPath path ^. _1
    (i,) <$> dSyncOneImpl path
  pure $ M.fromList rdata

dUpdateMetadataImpl :: (MonadKorrvigs m) => Event -> Map Text Value -> [Text] -> m ()
dUpdateMetadataImpl event upd rm = do
  let path = event ^. eventFile
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
