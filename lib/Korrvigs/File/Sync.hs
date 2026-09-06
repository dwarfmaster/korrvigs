{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.File.Sync where

import Control.Lens hiding ((.=))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Default
import Data.List hiding (insert)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.SQL
import Korrvigs.Compute.Type
import Korrvigs.Entry
import qualified Korrvigs.Entry.JSON as Gen
import Korrvigs.File.Computation
import Korrvigs.File.SQL
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Utils (recursiveRemoveFile, resolveSymbolicLink)
import Korrvigs.Utils.Crypto
import Korrvigs.Utils.DateTree
import Opaleye (Insert (..), doNothing, rCount, toFields)
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

data FileMetadata = FileMetadata
  { _savedMime :: Text,
    _genData :: Gen.EntryJSON,
    _computations :: Map Text ComputationResult
  }

makeLenses ''FileMetadata

parseCompResult :: Value -> Parser ComputationResult
parseCompResult = withObject "Computation result" $ \obj -> do
  tp <- maybe (fail "Unknown type") pure . parseTypeName =<< obj .: "type"
  ComputationResult tp
    <$> (maybe (fail "Invalid hash") pure . digestFromText =<< obj .: "hash")
    <*> (iso8601ParseM =<< obj .: "date")
    <*> obj .: "time"
    <*> (maybe (fail "Can't parse result") pure . decodeFromJson tp =<< obj .: "result")

compResultToJSON :: ComputationResult -> Value
compResultToJSON (ComputationResult tp hash date runtime res) =
  object
    [ "type" .= runTypeName tp,
      "hash" .= digestToText hash,
      "date" .= iso8601Show date,
      "time" .= runtime,
      "result" .= encodeToJSON res
    ]

instance FromJSON FileMetadata where
  parseJSON (Object v) =
    FileMetadata
      <$> v .: "mime"
      <*> Gen.parseObject v
      <*> (mapM parseCompResult . fromMaybe def =<< v .:? "computations")
  parseJSON invalid =
    prependFailure "parsing file metadata failed, " $ typeMismatch "Object" invalid

instance ToJSON FileMetadata where
  toJSON mtdt =
    object $
      [ "mime" .= (mtdt ^. savedMime)
      ]
        ++ addComps (mtdt ^. computations)
        ++ Gen.toObjectPairs (mtdt ^. genData)
    where
      addComps cmps =
        ["computations" .= (compResultToJSON <$> cmps) | not (M.null cmps)]

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

prepComp :: FileMetadata -> Text -> Runnable -> SyncComputationData
prepComp json code rbl =
  (Nothing, view cmpResDate <$> result, view cmpResRuntime <$> result, rbl ^. runType, deps)
  where
    deps = runDeps rbl
    result = M.lookup code $ json ^. computations

syncOne :: (MonadKorrvigs m) => Id -> FilePath -> Int -> m SyncData
syncOne i path sqlI = do
  let meta = metaPath path
  json <- liftIO (eitherDecode <$> readFile meta) >>= throwEither (KCantLoad i . T.pack)
  let mime = Enc.encodeUtf8 $ json ^. savedMime
  let cmps = M.mapWithKey (prepComp json) $ fileComputations i mime
  status <- liftIO $ computeStatus path
  let frow = FileRow sqlI path (metaPath path) status mime :: FileRow
  let insert =
        Insert
          { iTable = filesTable,
            iRows = [toFields frow],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  sdt <- Gen.syncJsonEntry i sqlI json [insert]
  pure $ sdt & syncCompute .~ cmps

updateImpl :: (MonadKorrvigs m) => File -> (FileMetadata -> m FileMetadata) -> m ()
updateImpl file f = do
  let meta = file ^. fileMeta
  json <- liftIO (eitherDecode <$> readFile meta) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile meta $ encodePretty $ gcComps njson
  where
    i = file ^. fileEntry . entryName
    gcComps meta =
      let ncomps =
            M.intersection (meta ^. computations) $
              fileComputations i (file ^. fileMime)
       in meta & computations .~ ncomps

instance Gen.JsonEntry FileMetadata File where
  genericJson = genData
  genericKind = const File
  genericUpdateImpl = updateImpl

updateMetadata :: (MonadKorrvigs m) => File -> Map Text Value -> [Text] -> m ()
updateMetadata = Gen.updateMetadata

updateParents :: (MonadKorrvigs m) => File -> [Id] -> [Id] -> m ()
updateParents = Gen.updateParents

updateDate :: (MonadKorrvigs m) => File -> Maybe ZonedTime -> m ()
updateDate = Gen.updateDate

updateDuration :: (MonadKorrvigs m) => File -> Maybe CalendarDiffTime -> m ()
updateDuration = Gen.updateDuration

updateRef :: (MonadKorrvigs m) => File -> Id -> Maybe Id -> m ()
updateRef = Gen.updateRef id

updateTitle :: (MonadKorrvigs m) => File -> Maybe Text -> m ()
updateTitle = Gen.updateTitle

getComputation :: (MonadKorrvigs m) => File -> Text -> m (Maybe Computation)
getComputation file cmp = case M.lookup cmp comps of
  Nothing -> pure Nothing
  Just rbl -> do
    json <- liftIO (eitherDecode <$> readFile (file ^. fileMeta)) >>= throwEither (KCantLoad i . T.pack)
    pure $
      Just $
        Computation
          { _cmpEntry = i,
            _cmpName = cmp,
            _cmpRun = rbl,
            _cmpResult = json ^. computations . at cmp,
            _cmpAutorun = Nothing
          }
  where
    i = file ^. fileEntry . entryName
    comps = fileComputations i (file ^. fileMime)

storeComputationResult :: (MonadKorrvigs m) => File -> Text -> RunnableType -> Hash -> UTCTime -> Int -> RunnableResult -> m ()
storeComputationResult file cmp tp hash date time res =
  updateImpl file $ pure . (computations . at cmp ?~ ComputationResult tp hash date time res)

clearComputationsResult :: (MonadKorrvigs m) => File -> [Text] -> m ()
clearComputationsResult file cmps =
  updateImpl file $ pure . (computations %~ (`M.difference` (M.fromList $ (,()) <$> cmps)))
