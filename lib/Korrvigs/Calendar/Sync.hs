module Korrvigs.Calendar.Sync where

import Control.Arrow (first, (&&&))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Int (Int64)
import Korrvigs.Calendar.JSON
import Korrvigs.Calendar.SQL
import Korrvigs.Compute
import Korrvigs.Compute.Builtin (Action (CalDav))
import Korrvigs.Entry
import Korrvigs.FTS
import Korrvigs.Kind
import Korrvigs.KindData (RelData (..))
import Korrvigs.Monad
import Opaleye hiding (not)
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

calIdFromPath :: FilePath -> Id
calIdFromPath = MkId . T.pack . takeBaseName

calBasename :: Id -> FilePath
calBasename cal = T.unpack $ unId cal <> ".json"

calendarPath' :: (MonadKorrvigs m) => Id -> m FilePath
calendarPath' cal = do
  rt <- calJSONPath
  pure $ joinPath [rt, calBasename cal]

calendarPath :: (MonadKorrvigs m) => Calendar -> m FilePath
calendarPath = calendarPath' . view (calEntry . name)

syncCalJSON :: (MonadKorrvigs m) => Id -> CalJSON -> m (EntryRow, CalRow)
syncCalJSON i json = do
  let mtdt = json ^. cljsMetadata
  let tm = json ^. cljsDate
  let dur = json ^. cljsDuration
  let geom = json ^. cljsGeo
  let erow = EntryRow i Calendar tm dur geom Nothing :: EntryRow
  let mtdtrows = uncurry (MetadataRow i) . first CI.mk <$> M.toList mtdt :: [MetadataRow]
  let crow = CalRow i (json ^. cljsServer) (json ^. cljsUser) (json ^. cljsCalName) :: CalRow
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
          { iTable = calendarsTable,
            iRows = [toFields crow],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    void $
      runInsert conn $
        Insert
          { iTable = entriesMetadataTable,
            iRows = toFields <$> mtdtrows,
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    case json ^. cljsText of
      Nothing -> pure ()
      Just t ->
        void $
          runUpdate conn $
            Update
              { uTable = entriesTable,
                uUpdateWith =
                  sqlEntryText .~ toNullable (tsParseEnglish $ sqlStrictText t),
                uWhere = \row -> row ^. sqlEntryName .== sqlId i,
                uReturning = rCount
              }
  pure (erow, crow)

syncCal :: (MonadKorrvigs m) => FilePath -> m RelData
syncCal path = do
  let i = calIdFromPath path
  prev <- load i
  forM_ prev dispatchRemoveDB
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  void $ syncCalJSON i json
  pure $
    RelData
      { _relSubOf = MkId <$> json ^. cljsParents,
        _relRefTo = []
      }

allCalendars :: (MonadKorrvigs m) => m [FilePath]
allCalendars = do
  rt <- calJSONPath
  files <- liftIO $ listDirectory rt
  pure $ joinPath . (\f -> [rt, f]) <$> files

dListImpl :: (MonadKorrvigs m) => m (Set FilePath)
dListImpl = S.fromList <$> allCalendars

dGetIdImpl :: FilePath -> Id
dGetIdImpl = calIdFromPath

dSyncImpl :: (MonadKorrvigs m) => m (Map Id (RelData, EntryComps))
dSyncImpl =
  M.fromList <$> (allCalendars >>= mapM (sequence . (calIdFromPath &&& dSyncOneImpl)))

dSyncOneImpl :: (MonadKorrvigs m) => FilePath -> m (RelData, EntryComps)
dSyncOneImpl path = do
  relData <- syncCal path
  let i = calIdFromPath path
  let cmps = M.singleton "dav" (Computation i "dav" (Builtin CalDav) Json)
  pure (relData, cmps)

dRemoveDBImpl :: Id -> [Delete Int64]
dRemoveDBImpl i =
  [ Delete
      { dTable = calendarsTable,
        dWhere = \crow -> crow ^. sqlCalName .== sqlId i,
        dReturning = rCount
      }
  ]

dRemoveImpl :: (MonadKorrvigs m) => FilePath -> m ()
dRemoveImpl path = do
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path

calFromRow :: CalRow -> Entry -> Calendar
calFromRow row entry =
  MkCalendar
    { _calEntry = entry,
      _calServer = row ^. sqlCalServer,
      _calUser = row ^. sqlCalUser,
      _calName = row ^. sqlCalCalName
    }

dLoadImpl :: (MonadKorrvigs m) => Id -> ((Entry -> Calendar) -> Entry) -> m (Maybe Entry)
dLoadImpl i cstr = do
  sel <- rSelectOne $ do
    crow <- selectTable calendarsTable
    where_ $ crow ^. sqlCalName .== sqlId i
    pure crow
  case (sel :: Maybe CalRow) of
    Nothing -> pure Nothing
    Just crow -> pure $ Just $ cstr $ calFromRow crow

dUpdateImpl :: (MonadKorrvigs m) => Calendar -> (CalJSON -> m CalJSON) -> m ()
dUpdateImpl cal f = do
  path <- calendarPath cal
  let i = cal ^. calEntry . name
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile path $ encode njson

dUpdateMetadataImpl :: (MonadKorrvigs m) => Calendar -> Map Text Value -> [Text] -> m ()
dUpdateMetadataImpl cal upd rm =
  dUpdateImpl cal $ pure . (cljsMetadata %~ M.union upd . flip (foldr M.delete) rm)

dUpdateParentsImpl :: (MonadKorrvigs m) => Calendar -> [Id] -> [Id] -> m ()
dUpdateParentsImpl cal toAdd toRm = dUpdateImpl cal $ pure . updParents
  where
    rmTxt = unId <$> toRm
    addTxt = unId <$> toAdd
    updParents = cljsParents %~ (addTxt ++) . filter (not . flip elem rmTxt)
