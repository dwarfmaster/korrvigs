module Korrvigs.Link.Sync where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Aeson (Value, eitherDecode, encode)
import Data.ByteString.Lazy (readFile)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime (localDay, zonedTimeToLocalTime)
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData (RelData (..))
import Korrvigs.Link.JSON
import Korrvigs.Link.SQL
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Utils.DateTree (listFiles, storeFile)
import Opaleye
import System.Directory (doesFileExist, removeFile)
import System.FilePath (takeBaseName)
import Prelude hiding (readFile)

linkIdFromPath :: FilePath -> Id
linkIdFromPath = MkId . T.pack . takeBaseName

syncLinkJSON :: (MonadKorrvigs m) => Id -> FilePath -> LinkJSON -> m (EntryRow, LinkRow)
syncLinkJSON i path json = do
  let mtdt = json ^. lkjsMetadata
  let extras = mtdtExtras mtdt
  let geom = extras ^. mtdtGeometry
  let tm = extras ^. mtdtDate
  let dur = extras ^. mtdtDuration
  let erow = EntryRow i Link tm dur geom Nothing :: EntryRow
  let mtdtrows = (\(key, val) -> MetadataRow i key val False) <$> M.toList mtdt :: [MetadataRow]
  let lrow = LinkRow i (json ^. lkjsProtocol) (json ^. lkjsLink) path :: LinkRow
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
          { iTable = linksTable,
            iRows = [toFields lrow],
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
  pure (erow, lrow)

syncLink :: (MonadKorrvigs m) => FilePath -> m RelData
syncLink path = do
  let i = linkIdFromPath path
  prev <- load i
  case prev of
    Nothing -> pure ()
    Just prevEntry ->
      case prevEntry ^. kindData of
        LinkD lnk | lnk ^. linkPath == path -> dispatchRemoveDB prevEntry
        _ -> dispatchRemove prevEntry
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  void $ syncLinkJSON i path json
  pure $
    RelData
      { _relSubOf = MkId <$> json ^. lkjsParents,
        _relRefTo = []
      }

allJSONs :: (MonadKorrvigs m) => m [FilePath]
allJSONs = do
  rt <- linkJSONPath
  let dtt = linkJSONTreeType
  files <- listFiles rt dtt
  pure $ (^. _1) <$> files

dListImpl :: (MonadKorrvigs m) => m (Set FilePath)
dListImpl = S.fromList <$> allJSONs

dGetIdImpl :: FilePath -> Id
dGetIdImpl = linkIdFromPath

dSyncImpl :: (MonadKorrvigs m) => f Link -> m (Map Id RelData)
dSyncImpl _ =
  M.fromList <$> (allJSONs >>= mapM (sequence . (linkIdFromPath &&& syncLink)))

dSyncOneImpl :: (MonadKorrvigs m) => FilePath -> m RelData
dSyncOneImpl = syncLink

dRemoveDBImpl :: Id -> [Delete Int64]
dRemoveDBImpl i =
  [ Delete
      { dTable = linksTable,
        dWhere = \lrow -> lrow ^. sqlLinkName .== sqlId i,
        dReturning = rCount
      }
  ]

dRemoveImpl :: (MonadKorrvigs m) => FilePath -> m ()
dRemoveImpl path = do
  let i = linkIdFromPath path
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path
  removeDB i

linkFromRow :: LinkRow -> Entry -> Link
linkFromRow row entry =
  MkLink
    { _linkEntry = entry,
      _linkProtocol = row ^. sqlLinkProtocol,
      _linkRef = row ^. sqlLinkRef,
      _linkPath = row ^. sqlLinkFile
    }

dLoadImpl :: (MonadKorrvigs m) => Id -> ((Entry -> Link) -> Entry) -> m (Maybe Entry)
dLoadImpl i cstr = do
  sel <- rSelectOne $ do
    lrow <- selectTable linksTable
    where_ $ lrow ^. sqlLinkName .== sqlId i
    pure lrow
  case (sel :: Maybe LinkRow) of
    Nothing -> pure Nothing
    Just lrow -> do
      pure $ Just $ cstr $ linkFromRow lrow

data LinkMaker = LinkMaker
  { _lkId :: IdMaker,
    _lkProtocol :: Text,
    _lkLink :: Text,
    _lkMtdt :: Map Text Value
  }
  deriving (Show)

makeLenses ''LinkMaker

lmk :: Text -> Text -> Text -> LinkMaker
lmk title prot lk = LinkMaker (imk "link" & idTitle ?~ title) prot lk M.empty

newLink :: (MonadKorrvigs m) => LinkMaker -> m Link
newLink mk = do
  i <- newId $ mk ^. lkId
  -- Create JSON file
  let json =
        LinkJSON
          { _lkjsProtocol = mk ^. lkProtocol,
            _lkjsLink = mk ^. lkLink,
            _lkjsMetadata = mk ^. lkMtdt,
            _lkjsParents = []
          }
  rt <- linkJSONPath
  let dt = mk ^. lkMtdt . to mtdtExtras . mtdtDate
  let day = localDay . zonedTimeToLocalTime <$> dt
  path <- storeFile rt linkJSONTreeType day (unId i <> ".json") $ encode json
  let metaValues = (\(key, val) -> (key, val, False)) <$> M.toList (mk ^. lkMtdt)
  -- Insert into database
  (erow, lrow) <- syncLinkJSON i path json
  -- Create haskell objects
  pure $ entryFromRow LinkD erow metaValues $ linkFromRow lrow
