module Korrvigs.Link.Sync where

import Control.Arrow (first, (&&&))
import Control.Lens
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class
import Data.Aeson (Value, eitherDecode, encode)
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.FTS
import Korrvigs.Kind
import Korrvigs.KindData (RelData (..))
import Korrvigs.Link.JSON
import Korrvigs.Link.SQL
import Korrvigs.Monad
import Korrvigs.Utils (recursiveRemoveFile)
import Korrvigs.Utils.DateTree (listFiles)
import Opaleye hiding (not)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName)
import Prelude hiding (readFile, writeFile)

linkIdFromPath :: FilePath -> Id
linkIdFromPath = MkId . T.pack . takeBaseName

syncLinkJSON :: (MonadKorrvigs m) => Id -> FilePath -> LinkJSON -> m (EntryRow, LinkRow)
syncLinkJSON i path json = do
  let mtdt = json ^. lkjsMetadata
  let tm = json ^. lkjsDate
  let dur = json ^. lkjsDuration
  let geom = json ^. lkjsGeo
  let erow = EntryRow i Link tm dur geom Nothing :: EntryRow
  let mtdtrows = uncurry (MetadataRow i) . first CI.mk <$> M.toList mtdt :: [MetadataRow]
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
    case json ^. lkjsText of
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
  pure (erow, lrow)

syncLink :: (MonadKorrvigs m) => FilePath -> m RelData
syncLink path = do
  let i = linkIdFromPath path
  prev <- load i
  forM_ prev dispatchRemoveDB
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
  rt <- linkJSONPath
  exists <- liftIO $ doesFileExist path
  when exists $ recursiveRemoveFile rt path

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

dUpdateImpl :: (MonadKorrvigs m) => Link -> (LinkJSON -> m LinkJSON) -> m ()
dUpdateImpl link f = do
  let path = link ^. linkPath
  let i = link ^. linkEntry . name
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile path $ encode njson

dUpdateMetadataImpl :: (MonadKorrvigs m) => Link -> Map Text Value -> [Text] -> m ()
dUpdateMetadataImpl link upd rm = dUpdateImpl link $ pure . updMtdt
  where
    updMtdt = lkjsMetadata %~ M.union upd . flip (foldr M.delete) rm

dUpdateParentsImpl :: (MonadKorrvigs m) => Link -> [Id] -> [Id] -> m ()
dUpdateParentsImpl link toAdd toRm = dUpdateImpl link $ pure . updParents
  where
    rmTxt = unId <$> toRm
    addTxt = unId <$> toAdd
    updParents = lkjsParents %~ (addTxt ++) . filter (not . flip elem rmTxt)
