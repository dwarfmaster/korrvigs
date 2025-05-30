module Korrvigs.Link.Sync where

import Control.Arrow (first, (&&&))
import Control.Lens
import Control.Monad (when)
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
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Link.JSON
import Korrvigs.Link.SQL
import Korrvigs.Monad
import Korrvigs.Utils (recursiveRemoveFile)
import Korrvigs.Utils.DateTree (listFiles)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName)
import Prelude hiding (readFile, writeFile)

linksDirectory :: (MonadKorrvigs m) => m FilePath
linksDirectory = linkJSONPath

linkIdFromPath :: FilePath -> Id
linkIdFromPath = MkId . T.pack . takeBaseName

syncLinkJSON :: (MonadKorrvigs m) => Id -> FilePath -> LinkJSON -> m (SyncData LinkRow)
syncLinkJSON i path json = do
  let mtdt = json ^. lkjsMetadata
  let tm = json ^. lkjsDate
  let dur = json ^. lkjsDuration
  let geom = json ^. lkjsGeo
  let erow = EntryRow i Link tm dur geom Nothing :: EntryRow
  let mtdtrows = uncurry (MetadataRow i) . first CI.mk <$> M.toList mtdt :: [MetadataRow]
  let lrow = LinkRow i (json ^. lkjsProtocol) (json ^. lkjsLink) path :: LinkRow
  pure $ SyncData erow lrow mtdtrows (json ^. lkjsText) (MkId <$> json ^. lkjsParents) [] M.empty

syncLink :: (MonadKorrvigs m) => FilePath -> m (SyncData LinkRow)
syncLink path = do
  let i = linkIdFromPath path
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  syncLinkJSON i path json

allJSONs :: (MonadKorrvigs m) => m [FilePath]
allJSONs = do
  rt <- linkJSONPath
  let dtt = linkJSONTreeType
  files <- listFiles rt dtt
  pure $ (^. _1) <$> files

list :: (MonadKorrvigs m) => m (Set FilePath)
list = S.fromList <$> allJSONs

sync :: (MonadKorrvigs m) => m (Map Id (SyncData LinkRow))
sync =
  M.fromList <$> (allJSONs >>= mapM (sequence . (linkIdFromPath &&& syncLink)))

syncOne :: (MonadKorrvigs m) => FilePath -> m (SyncData LinkRow)
syncOne = syncLink

remove :: (MonadKorrvigs m) => Link -> m ()
remove lnk = do
  let path = lnk ^. linkPath
  rt <- linkJSONPath
  exists <- liftIO $ doesFileExist path
  when exists $ recursiveRemoveFile rt path

updateImpl :: (MonadKorrvigs m) => Link -> (LinkJSON -> m LinkJSON) -> m ()
updateImpl link f = do
  let path = link ^. linkPath
  let i = link ^. linkEntry . name
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile path $ encode njson

updateMetadata :: (MonadKorrvigs m) => Link -> Map Text Value -> [Text] -> m ()
updateMetadata link upd rm = updateImpl link $ pure . updMtdt
  where
    updMtdt = lkjsMetadata %~ M.union upd . flip (foldr M.delete) rm

updateParents :: (MonadKorrvigs m) => Link -> [Id] -> [Id] -> m ()
updateParents link toAdd toRm = updateImpl link $ pure . updParents
  where
    rmTxt = unId <$> toRm
    addTxt = unId <$> toAdd
    updParents = lkjsParents %~ (addTxt ++) . filter (not . flip elem rmTxt)
