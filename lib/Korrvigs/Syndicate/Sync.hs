module Korrvigs.Syndicate.Sync where

import Control.Arrow (first)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Foldable
import Data.List hiding (insert)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Syndicate.Item
import Korrvigs.Syndicate.JSON
import Korrvigs.Syndicate.SQL
import Korrvigs.Utils (recursiveRemoveFile)
import Korrvigs.Utils.DateTree
import Opaleye hiding (not, null)
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

synIdFromPath :: FilePath -> Id
synIdFromPath = MkId . T.pack . takeBaseName

synBasename :: Id -> FilePath
synBasename syn = T.unpack $ unId syn <> ".json"

syndicatesDirectory :: (MonadKorrvigs m) => m FilePath
syndicatesDirectory = synJSONPath

synTreeType :: DateTreeType
synTreeType = def & dtYear .~ True & dtMonth .~ True

remove :: (MonadKorrvigs m) => Syndicate -> m ()
remove syn = do
  let path = syn ^. synPath
  rt <- syndicatesDirectory
  exists <- liftIO $ doesFileExist path
  when exists $ recursiveRemoveFile rt path

allSyndicates :: (MonadKorrvigs m) => m [FilePath]
allSyndicates = do
  rt <- syndicatesDirectory
  let dtt = synTreeType
  files <- listFiles rt dtt
  pure $ view _1 <$> files

list :: (MonadKorrvigs m) => m (Set FilePath)
list = S.fromList <$> allSyndicates

syncOne :: (MonadKorrvigs m) => Id -> FilePath -> Int -> m SyncData
syncOne i path sqlI = do
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  syncSynJSON i path sqlI json

syncSynJSON :: (MonadKorrvigs m) => Id -> FilePath -> Int -> SyndicateJSON -> m SyncData
syncSynJSON i path sqlI json = do
  let mtdt = json ^. synjsMetadata
  let tm = json ^. synjsDate
  let dur = json ^. synjsDuration
  let geom = json ^. synjsGeo
  let title = json ^. synjsTitle
  let erow = EntryRow (Just sqlI) Syndicate i tm dur geom Nothing title :: EntryRowW
  let mtdtrows = first CI.mk <$> M.toList mtdt
  let renderFilter (MkId fId, fCode) = fId <> "#" <> fCode
  let srow = SyndicateRow sqlI (json ^. synjsUrl) path (json ^. synjsETag) (renderFilter <$> json ^. synjsFilters) (json ^. synjsExpiration) :: SyndicateRow
  let insert =
        Insert
          { iTable = syndicatesTable,
            iRows = [toFields srow],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  let irows = flip fmap (zip [1 ..] $ json ^. synjsItems) $ \(sq, item) -> SyndicateItemRow sqlI sq (item ^. synitTitle) (item ^. synitUrl) (item ^. synitRead) (item ^. synitGUID) (item ^. synitDate) :: SyndicateItemRow
  let insertItemRows =
        Insert
          { iTable = syndicatedItemsTable,
            iRows = toFields <$> irows,
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  let refs = json ^.. synjsFilters . each . _1
  pure $ SyncData erow [insert, insertItemRows] mtdtrows (json ^. synjsText) (MkId <$> json ^. synjsParents) refs M.empty

updateFile :: (MonadKorrvigs m) => Id -> FilePath -> (SyndicateJSON -> m SyndicateJSON) -> m ()
updateFile i path f = do
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile path $ encodePretty njson

updateImpl :: (MonadKorrvigs m) => Syndicate -> (SyndicateJSON -> m SyndicateJSON) -> m ()
updateImpl syn = updateFile (syn ^. synEntry . entryName) (syn ^. synPath)

updateMetadata :: (MonadKorrvigs m) => Syndicate -> Map Text Value -> [Text] -> m ()
updateMetadata syn upd rm =
  updateImpl syn $ pure . (synjsMetadata %~ M.union upd . flip (foldr M.delete) rm)

updateParents :: (MonadKorrvigs m) => Syndicate -> [Id] -> [Id] -> m ()
updateParents syn toAdd toRm = updateImpl syn $ pure . updParents
  where
    rmTxt = unId <$> toRm
    addTxt = unId <$> toAdd
    updParents = synjsParents %~ (addTxt ++) . filter (not . flip elem rmTxt)

updateDate :: (MonadKorrvigs m) => Syndicate -> Maybe ZonedTime -> m ()
updateDate syn ntime = updateImpl syn $ pure . (synjsDate .~ ntime)

updateDuration :: (MonadKorrvigs m) => Syndicate -> Maybe CalendarDiffTime -> m ()
updateDuration syn ndur = updateImpl syn $ pure . (synjsDuration .~ ndur)

updateRef :: (MonadKorrvigs m) => Syndicate -> Id -> Maybe Id -> m ()
updateRef syn old new =
  updateImpl syn $
    pure
      . (synjsParents %~ upd)
      . (synjsMetadata %~ updateInMetadata old new)
      . (synjsFilters %~ (>>= updFilter))
  where
    upd [] = []
    upd (p : ps) | p == unId old = maybe id ((:) . unId) new ps
    upd (p : ps) = p : upd ps
    updFilter (entry, code) | entry == old = (,code) <$> toList new
    updFilter (entry, code) = [(entry, code)]

updateTitle :: (MonadKorrvigs m) => Syndicate -> Maybe Text -> m ()
updateTitle syn ntitle = updateImpl syn $ pure . (synjsTitle .~ ntitle)

readItem :: (MonadKorrvigs m) => Syndicate -> Int -> m ()
readItem syn item = do
  let sqlI = syn ^. synEntry . entryId
  updateImpl syn $ pure . (synjsItems . ix (item - 1) . synitRead .~ True)
  atomicSQL $ \conn -> do
    void $
      runUpdate conn $
        Update
          { uTable = syndicatedItemsTable,
            uUpdateWith = sqlSynItRead .~ sqlBool True,
            uWhere = \row -> row ^. sqlSynItSyndicate .== sqlInt4 sqlI .&& (row ^. sqlSynItSequence) .== sqlInt4 item,
            uReturning = rCount
          }
