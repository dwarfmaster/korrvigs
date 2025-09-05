module Korrvigs.Calendar.Sync where

import Control.Arrow (first, (&&&))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Korrvigs.Calendar.JSON
import Korrvigs.Calendar.SQL
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

calIdFromPath :: FilePath -> Id
calIdFromPath = MkId . T.pack . takeBaseName

calBasename :: Id -> FilePath
calBasename cal = T.unpack $ unId cal <> ".json"

calendarsDirectory :: (MonadKorrvigs m) => m FilePath
calendarsDirectory = calJSONPath

calendarPath' :: (MonadKorrvigs m) => Id -> m FilePath
calendarPath' cal = do
  rt <- calJSONPath
  pure $ joinPath [rt, calBasename cal]

calendarPath :: (MonadKorrvigs m) => Calendar -> m FilePath
calendarPath = calendarPath' . view (calEntry . entryName)

syncCalJSON :: (MonadKorrvigs m) => Id -> CalJSON -> m (SyncData CalRow)
syncCalJSON i json = do
  let mtdt = json ^. cljsMetadata
  let tm = json ^. cljsDate
  let dur = json ^. cljsDuration
  let geom = json ^. cljsGeo
  let title = json ^. cljsTitle
  let erow = EntryRow Nothing Calendar i tm dur geom Nothing title :: EntryRowW
  let mtdtrows = first CI.mk <$> M.toList mtdt
  let crow sqlI = CalRow sqlI (json ^. cljsServer) (json ^. cljsUser) (json ^. cljsCalName) :: CalRow
  pure $ SyncData erow crow mtdtrows (json ^. cljsText) title (MkId <$> json ^. cljsParents) [] M.empty

syncOne :: (MonadKorrvigs m) => FilePath -> m (SyncData CalRow)
syncOne path = do
  let i = calIdFromPath path
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  syncCalJSON i json

allCalendars :: (MonadKorrvigs m) => m [FilePath]
allCalendars = do
  rt <- calJSONPath
  files <- liftIO $ listDirectory rt
  pure $ joinPath . (\f -> [rt, f]) <$> files

list :: (MonadKorrvigs m) => m (Set FilePath)
list = S.fromList <$> allCalendars

sync :: (MonadKorrvigs m) => m (Map Id (SyncData CalRow))
sync =
  M.fromList <$> (allCalendars >>= mapM (sequence . (calIdFromPath &&& syncOne)))

remove :: (MonadKorrvigs m) => Calendar -> m ()
remove cal = do
  path <- calendarPath cal
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path

updateFile :: (MonadKorrvigs m) => Id -> FilePath -> (CalJSON -> m CalJSON) -> m ()
updateFile i path f = do
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile path $ encode njson

updateImpl :: (MonadKorrvigs m) => Calendar -> (CalJSON -> m CalJSON) -> m ()
updateImpl cal f = do
  path <- calendarPath cal
  let i = cal ^. calEntry . entryName
  updateFile i path f

updateMetadata :: (MonadKorrvigs m) => Calendar -> Map Text Value -> [Text] -> m ()
updateMetadata cal upd rm =
  updateImpl cal $ pure . (cljsMetadata %~ M.union upd . flip (foldr M.delete) rm)

updateParents :: (MonadKorrvigs m) => Calendar -> [Id] -> [Id] -> m ()
updateParents cal toAdd toRm = updateImpl cal $ pure . updParents
  where
    rmTxt = unId <$> toRm
    addTxt = unId <$> toAdd
    updParents = cljsParents %~ (addTxt ++) . filter (not . flip elem rmTxt)

updateDate :: (MonadKorrvigs m) => Calendar -> Maybe ZonedTime -> m ()
updateDate cal ntime =
  updateImpl cal $ pure . (cljsDate .~ ntime)
