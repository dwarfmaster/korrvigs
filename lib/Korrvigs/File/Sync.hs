module Korrvigs.File.Sync where

import Control.Arrow ((&&&))
import Control.Lens hiding ((.=))
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.FTS
import Korrvigs.File.SQL
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Utils (resolveSymbolicLink)
import Korrvigs.Utils.DateTree
import Opaleye hiding (not)
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

data FileMetadata = FileMetadata
  { _savedMime :: Text,
    _extracted :: Map Text Value,
    _annoted :: Map Text Value
  }

makeLenses ''FileMetadata

instance FromJSON FileMetadata where
  parseJSON (Object v) =
    FileMetadata <$> v .: "mime" <*> v .: "extracted" <*> v .: "metadata"
  parseJSON invalid =
    prependFailure "parsing file metadata failed, " $ typeMismatch "Object" invalid

instance ToJSON FileMetadata where
  toJSON mtdt =
    object
      [ "extracted" .= (mtdt ^. extracted),
        "metadata" .= (mtdt ^. annoted),
        "mime" .= (mtdt ^. savedMime)
      ]

fileMetadataToMetadata :: FileMetadata -> Metadata
fileMetadataToMetadata fmtdt =
  mconcat
    [ mv True <$> (fmtdt ^. extracted),
      mv False <$> (fmtdt ^. annoted)
    ]
  where
    mv :: Bool -> Value -> MetadataValue
    mv = flip MValue

metaPath :: FilePath -> FilePath
metaPath = (<> ".meta")

isMeta :: FilePath -> Bool
isMeta p = takeExtension p == ".meta"

dGetIdImpl :: FilePath -> Id
dGetIdImpl = MkId . T.pack . takeBaseName

fileFromRow :: FileRow -> Entry -> File
fileFromRow frow entry =
  MkFile
    { _fileEntry = entry,
      _filePath = frow ^. sqlFilePath,
      _fileMeta = frow ^. sqlFileMeta,
      _fileStatus = frow ^. sqlFileStatus,
      _fileMime = frow ^. sqlFileMime
    }

dLoadImpl :: (MonadKorrvigs m) => Id -> ((Entry -> File) -> Entry) -> m (Maybe Entry)
dLoadImpl i cstr = do
  sel <- rSelectOne $ do
    frow <- selectTable filesTable
    where_ $ frow ^. sqlFileName .== sqlId i
    pure frow
  case (sel :: Maybe FileRow) of
    Nothing -> pure Nothing
    Just frow -> pure . Just . cstr $ fileFromRow frow

dRemoveDBImpl :: Id -> [Delete Int64]
dRemoveDBImpl i =
  [ Delete
      { dTable = filesTable,
        dWhere = \frow -> frow ^. sqlFileName .== sqlId i,
        dReturning = rCount
      }
  ]

dRemoveImpl :: (MonadKorrvigs m) => FilePath -> m ()
dRemoveImpl path = do
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path
  let meta = metaPath path
  existsMeta <- liftIO $ doesFileExist meta
  when existsMeta $ liftIO $ removeFile meta

filesDirectory :: (MonadKorrvigs m) => m FilePath
filesDirectory = joinPath . (: ["files"]) <$> root

filesTreeType :: DateTreeType
filesTreeType = def & dtYear .~ True & dtMonth .~ True & dtDay .~ True

allFiles :: (MonadKorrvigs m) => m [FilePath]
allFiles = do
  rt <- filesDirectory
  let dtt = filesTreeType
  files <- listFiles rt dtt
  pure . filter (not . isMeta) $ (^. _1) <$> files

dListImpl :: (MonadKorrvigs m) => m (Set FilePath)
dListImpl = S.fromList <$> allFiles

computeStatus :: FilePath -> IO FileStatus
computeStatus path = do
  sym <- pathIsSymbolicLink path
  if sym
    then do
      dest <- resolveSymbolicLink path
      ex <- doesFileExist dest
      pure $ if ex then FilePresent else FileAbsent
    else pure FilePlain

dSyncImpl :: (MonadKorrvigs m) => m (Map Id RelData)
dSyncImpl =
  M.fromList <$> (allFiles >>= mapM (sequence . (dGetIdImpl &&& dSyncOneImpl)))

dSyncOneImpl :: (MonadKorrvigs m) => FilePath -> m RelData
dSyncOneImpl path = do
  let i = dGetIdImpl path
  prev <- load i
  case prev of
    Nothing -> pure ()
    Just prevEntry ->
      case prevEntry ^. kindData of
        FileD file | file ^. filePath == path -> dispatchRemoveDB prevEntry
        _ -> dispatchRemove prevEntry
  let meta = metaPath path
  json <- liftIO (eitherDecode <$> readFile meta) >>= throwEither (KCantLoad i . T.pack)
  let mtdt = fileMetadataToMetadata json
  let extras = mtdtExtras $ (^. metaValue) <$> mtdt
  let mime = Enc.encodeUtf8 $ json ^. savedMime
  status <- liftIO $ computeStatus path
  let geom = extras ^. mtdtGeometry
  let tm = extras ^. mtdtDate
  let dur = extras ^. mtdtDuration
  let erow = EntryRow i File tm dur geom Nothing :: EntryRow
  let mtdtrows = (\(key, val) -> MetadataRow i key (val ^. metaValue) (val ^. metaReadOnly)) <$> M.toList mtdt :: [MetadataRow]
  let frow = FileRow i path (metaPath path) status mime :: FileRow
  let txt = extras ^. mtdtText
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
          { iTable = filesTable,
            iRows = [toFields frow],
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
    case txt of
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
  pure $
    RelData
      { _relSubOf = fromMaybe [] $ extras ^. mtdtParents,
        _relRefTo = []
      }

dUpdateMetadataImpl :: (MonadKorrvigs m) => File -> Map Text Value -> [Text] -> m ()
dUpdateMetadataImpl file upd rm = do
  let i = file ^. fileEntry . name
  let meta = file ^. fileMeta
  json <- liftIO (eitherDecode <$> readFile meta) >>= throwEither (KCantLoad i . T.pack)
  let njson = json & annoted %~ M.union upd . flip (foldr M.delete) rm
  liftIO $ writeFile meta $ encode njson
