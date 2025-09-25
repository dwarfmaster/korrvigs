module Korrvigs.Link.Sync where

import Control.Arrow (first, (&&&))
import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Aeson (Value, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List hiding (insert)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Link.JSON
import Korrvigs.Link.SQL
import Korrvigs.Monad
import Korrvigs.Utils (recursiveRemoveFile)
import Korrvigs.Utils.DateTree (listFiles)
import Opaleye (Insert (..), doNothing, rCount, toFields)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName)
import Prelude hiding (readFile, writeFile)

linksDirectory :: (MonadKorrvigs m) => m FilePath
linksDirectory = linkJSONPath

linkIdFromPath :: FilePath -> Id
linkIdFromPath = MkId . T.pack . takeBaseName

syncLinkJSON :: (MonadKorrvigs m) => Id -> FilePath -> LinkJSON -> m SyncData
syncLinkJSON i path json = do
  let mtdt = json ^. lkjsMetadata
  let tm = json ^. lkjsDate
  let dur = json ^. lkjsDuration
  let geom = json ^. lkjsGeo
  let title = json ^. lkjsTitle
  let erow = EntryRow Nothing Link i tm dur geom Nothing title :: EntryRowW
  let mtdtrows = first CI.mk <$> M.toList mtdt :: [(CI Text, Value)]
  let lrow sqlI = LinkRow sqlI (json ^. lkjsProtocol) (json ^. lkjsLink) path
  let insert sqlI =
        Insert
          { iTable = linksTable,
            iRows = [toFields $ lrow sqlI],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  pure $ SyncData erow (singleton . insert) mtdtrows (json ^. lkjsText) title (MkId <$> json ^. lkjsParents) [] M.empty

syncLink :: (MonadKorrvigs m) => FilePath -> m SyncData
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

sync :: (MonadKorrvigs m) => m (Map Id SyncData)
sync =
  M.fromList <$> (allJSONs >>= mapM (sequence . (linkIdFromPath &&& syncLink)))

syncOne :: (MonadKorrvigs m) => FilePath -> m SyncData
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
  let i = link ^. linkEntry . entryName
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile path $ encodePretty njson

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

updateDate :: (MonadKorrvigs m) => Link -> Maybe ZonedTime -> m ()
updateDate link ntime = updateImpl link $ pure . (lkjsDate .~ ntime)

updateRef :: (MonadKorrvigs m) => Link -> Id -> Maybe Id -> m ()
updateRef link old new = updateImpl link $ pure . (lkjsParents %~ upd) . (lkjsMetadata %~ updateInMetadata old new)
  where
    upd = maybe id (\i -> (unId i :)) new . filter (/= unId old)

updateTitle :: (MonadKorrvigs m) => Link -> Maybe Text -> m ()
updateTitle link ntitle = updateImpl link $ pure . (lkjsTitle .~ ntitle)
