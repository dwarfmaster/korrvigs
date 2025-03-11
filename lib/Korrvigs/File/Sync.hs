module Korrvigs.File.Sync where

import Control.Arrow (first, (&&&))
import Control.Lens hiding ((.=))
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time.LocalTime
import Korrvigs.Actions.SQL
import Korrvigs.Compute
import Korrvigs.Compute.Builtin
import Korrvigs.Entry
import Korrvigs.FTS
import Korrvigs.File.SQL
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Monad
import Korrvigs.Utils (recursiveRemoveFile, resolveSymbolicLink)
import Korrvigs.Utils.DateTree
import Network.Mime
import Opaleye hiding (not)
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

data FileMetadata = FileMetadata
  { _savedMime :: Text,
    _annoted :: Map Text Value,
    _exDate :: Maybe ZonedTime,
    _exDuration :: Maybe CalendarDiffTime,
    _exGeo :: Maybe Geometry,
    _exText :: Maybe Text,
    _exParents :: [Id]
  }

makeLenses ''FileMetadata

instance FromJSON FileMetadata where
  parseJSON (Object v) =
    FileMetadata
      <$> v .: "mime"
      <*> v .: "metadata"
      <*> v .:? "date"
      <*> v .:? "duration"
      <*> v .:? "geometry"
      <*> v .:? "textContent"
      <*> (fmap MkId <$> v .: "parents")
  parseJSON invalid =
    prependFailure "parsing file metadata failed, " $ typeMismatch "Object" invalid

instance ToJSON FileMetadata where
  toJSON mtdt =
    object $
      [ "metadata" .= (mtdt ^. annoted),
        "mime" .= (mtdt ^. savedMime),
        "parents" .= (unId <$> mtdt ^. exParents)
      ]
        ++ maybe [] ((: []) . ("date" .=)) (mtdt ^. exDate)
        ++ maybe [] ((: []) . ("duration" .=)) (mtdt ^. exDuration)
        ++ maybe [] ((: []) . ("geometry" .=)) (mtdt ^. exGeo)
        ++ maybe [] ((: []) . ("textContent" .=)) (mtdt ^. exText)

metaPath :: FilePath -> FilePath
metaPath = (<> ".meta")

isMeta :: FilePath -> Bool
isMeta p = takeExtension p == ".meta"

dGetIdImpl :: FilePath -> Id
dGetIdImpl = MkId . T.pack . takeBaseName

remove :: (MonadKorrvigs m) => File -> m ()
remove file = do
  let path = file ^. filePath
  rt <- filesDirectory
  exists <- liftIO $ doesFileExist path
  when exists $ recursiveRemoveFile rt path
  let meta = metaPath path
  existsMeta <- liftIO $ doesFileExist meta
  when existsMeta $ recursiveRemoveFile rt meta

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

computeFromMime :: Id -> MimeType -> EntryComps
computeFromMime i mime = cmp $ Enc.decodeASCII mime
  where
    cmp m
      | S.member m scalars = miniature ScalarImage
      | T.isPrefixOf "image/" m =
          miniature Picture
            <> M.singleton "size" (Computation i "size" (Builtin Size) Json)
      | T.isPrefixOf "video/" m = miniature Picture
      | otherwise = M.empty
    miniature = M.singleton "miniature" . Computation i "miniature" (Builtin Miniature)
    scalars = S.fromList ["image/apng", "image/png", "image/bmp"]

listCompute :: (MonadKorrvigs m) => File -> m EntryComps
listCompute file = do
  let path = file ^. filePath
  let i = file ^. fileEntry . name
  let meta = metaPath path
  json <- liftIO (eitherDecode <$> readFile meta) >>= throwEither (KCantLoad i . T.pack)
  let mime = Enc.encodeUtf8 $ json ^. savedMime
  pure $ computeFromMime i mime

dSyncImpl :: (MonadKorrvigs m) => m (Map Id (RelData, EntryComps))
dSyncImpl =
  M.fromList <$> (allFiles >>= mapM (sequence . (dGetIdImpl &&& dSyncOneImpl)))

dSyncOneImpl :: (MonadKorrvigs m) => FilePath -> m (RelData, EntryComps)
dSyncOneImpl path = do
  let i = dGetIdImpl path
  prev <- load i
  forM_ prev removeDB
  let meta = metaPath path
  json <- liftIO (eitherDecode <$> readFile meta) >>= throwEither (KCantLoad i . T.pack)
  let mtdt = json ^. annoted
  let mime = Enc.encodeUtf8 $ json ^. savedMime
  let cmps = computeFromMime i mime
  status <- liftIO $ computeStatus path
  let geom = json ^. exGeo
  let tm = json ^. exDate
  let dur = json ^. exDuration
  let erow = EntryRow i File tm dur geom Nothing :: EntryRow
  let mtdtrows = uncurry (MetadataRow i) . first CI.mk <$> M.toList mtdt :: [MetadataRow]
  let frow = FileRow i path (metaPath path) status mime :: FileRow
  let txt = json ^. exText
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
  pure
    ( RelData
        { _relSubOf = json ^. exParents,
          _relRefTo = []
        },
      cmps
    )

updateImpl :: (MonadKorrvigs m) => File -> (FileMetadata -> m FileMetadata) -> m ()
updateImpl file f = do
  let i = file ^. fileEntry . name
  let meta = file ^. fileMeta
  json <- liftIO (eitherDecode <$> readFile meta) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile meta $ encode njson

updateMetadata :: (MonadKorrvigs m) => File -> Map Text Value -> [Text] -> m ()
updateMetadata file upd rm = do
  updateImpl file $ pure . (annoted %~ M.union upd . flip (foldr M.delete) rm)

updateParents :: (MonadKorrvigs m) => File -> [Id] -> [Id] -> m ()
updateParents file toAdd toRm =
  updateImpl file $ pure . (exParents %~ updParents)
  where
    updParents = (toAdd ++) . filter (not . flip elem toRm)
