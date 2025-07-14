module Korrvigs.File.Sync where

import Control.Arrow (first, (&&&))
import Control.Lens hiding ((.=))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
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
import qualified Korrvigs.Compute.Action as Act
import Korrvigs.Compute.Builtin
import Korrvigs.Entry
import Korrvigs.File.SQL
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Utils (recursiveRemoveFile, resolveSymbolicLink)
import Korrvigs.Utils.DateTree
import Network.Mime
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

fileIdFromPath :: FilePath -> Id
fileIdFromPath = MkId . T.pack . takeBaseName

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

list :: (MonadKorrvigs m) => m (Set FilePath)
list = S.fromList <$> allFiles

computeStatus :: FilePath -> IO FileStatus
computeStatus path = do
  sym <- pathIsSymbolicLink path
  if sym
    then do
      dest <- resolveSymbolicLink path
      ex <- doesFileExist dest
      pure $ if ex then FilePresent else FileAbsent
    else pure FilePlain

computeFromMime :: Id -> MimeType -> Map Text Act.Action
computeFromMime i mime = cmp $ Enc.decodeASCII mime
  where
    cmp m
      | T.isPrefixOf "image/" m || T.isPrefixOf "video/" m =
          miniature
            <> M.singleton "size" (Act.Builtin i Size)
      | otherwise = M.empty
    miniature = M.singleton "miniature" $ Act.Builtin i Miniature

sync :: (MonadKorrvigs m) => m (Map Id (SyncData FileRow))
sync =
  M.fromList <$> (allFiles >>= mapM (sequence . (fileIdFromPath &&& syncOne)))

syncOne :: (MonadKorrvigs m) => FilePath -> m (SyncData FileRow)
syncOne path = do
  let i = fileIdFromPath path
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
  let sdt = SyncData erow frow mtdtrows txt (json ^. exParents) [] cmps
  pure sdt

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
