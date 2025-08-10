module Korrvigs.Event.Sync where

import Conduit (throwM)
import Control.Arrow ((***))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (Result (Error, Success), Value, fromJSON, toJSON)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Event.ICalendar
import Korrvigs.Event.SQL
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Utils (recursiveRemoveFile)
import Korrvigs.Utils.DateTree
import System.Directory (doesFileExist)
import System.FilePath (joinPath, takeBaseName)

eventTreeType :: DateTreeType
eventTreeType = def & dtYear .~ True & dtMonth .~ True

eventIdFromPath :: FilePath -> (Id, Id)
eventIdFromPath path = MkId *** (MkId . T.drop 1) $ T.breakOn "_" $ T.pack $ takeBaseName path

remove :: (MonadKorrvigs m) => Event -> m ()
remove ev = do
  let path = ev ^. eventFile
  rt <- eventsDirectory
  exists <- liftIO $ doesFileExist path
  when exists $ recursiveRemoveFile rt path

eventsDirectory :: (MonadKorrvigs m) => m FilePath
eventsDirectory = joinPath . (: ["events"]) <$> root

allEvents :: (MonadKorrvigs m) => m [FilePath]
allEvents = do
  rt <- eventsDirectory
  let dtt = eventTreeType
  files <- listFiles rt dtt
  pure $ view _1 <$> files

list :: (MonadKorrvigs m) => m (Set FilePath)
list = S.fromList <$> allEvents

createIdFor :: (MonadKorrvigs m) => ICalFile -> ICalEvent -> m Id
createIdFor ical ievent = do
  let language = extractMtdt Language $ ievent ^. iceMtdt
  let summary = ievent ^. iceSummary
  let startSpec = ievent ^? iceStart . _Just
  let start = resolveICalTime ical <$> startSpec
  let parents = ievent ^. iceParents
  newId $
    imk "ics"
      & idTitle .~ summary
      & idDate .~ start
      & idLanguage ?~ fromMaybe "fr" language
      & idParent .~ listToMaybe parents

register :: (MonadKorrvigs m) => ICalFile -> m Id
register ical =
  case ical ^. icEvent of
    Nothing -> throwM $ KMiscError "ics has no event"
    Just ievent -> createIdFor ical ievent

syncEvent :: (MonadKorrvigs m) => Id -> Id -> FilePath -> ICalFile -> ICalEvent -> m (SyncData EventRow)
syncEvent i calendar ics ifile ical = do
  let extractTitle = maybe id (M.insert (mtdtName Title) . toJSON) $ ical ^. iceSummary
  let mtdt = extractTitle $ ical ^. iceMtdt
  let geom = ical ^. iceGeometry
  let tm = resolveICalTime ifile <$> ical ^. iceStart
  let dur = case (tm, ical ^. iceEnd) of
        (Just st, Just nd) ->
          let ndTime = resolveICalTime ifile nd
           in let diff = diffUTCTime (zonedTimeToUTC ndTime) (zonedTimeToUTC st)
               in Just $ calendarTimeTime diff
        _ -> calendarTimeTime <$> ical ^. iceDuration
  let erow = EntryRow i Event tm dur geom Nothing :: EntryRow
  let mrows = uncurry (MetadataRow i) <$> M.toList mtdt :: [MetadataRow]
  let evrow = EventRow i calendar ics (ical ^. iceUid) :: EventRow
  let txt = T.intercalate " " $ catMaybes [ical ^. iceComment, ical ^. iceSummary, ical ^. iceDescription]
  let txt' = if T.null txt then Nothing else Just txt
  pure $ SyncData erow evrow mrows txt' (calendar : ical ^. iceParents) [] M.empty

syncOneEvent :: (MonadKorrvigs m) => Id -> Id -> FilePath -> ICalFile -> ICalEvent -> m (SyncData EventRow)
syncOneEvent i calendar ics ifile ievent = do
  prev <- load i
  forM_ prev removeDB
  syncEvent i calendar ics ifile ievent

syncOne :: (MonadKorrvigs m) => FilePath -> m (SyncData EventRow)
syncOne path = do
  let (i, calendar) = eventIdFromPath path
  liftIO (parseICalFile path) >>= \case
    Left err -> throwM $ KMiscError $ "Failed to parse \"" <> T.pack path <> "\": " <> err
    Right ifile -> case ifile ^. icEvent of
      Nothing -> throwM $ KMiscError $ "Ics file \"" <> T.pack path <> "\" has no VEVENT"
      Just ievent -> syncOneEvent i calendar path ifile ievent

sync :: (MonadKorrvigs m) => m (Map Id (SyncData EventRow))
sync = do
  files <- allEvents
  rdata <- forM files $ \path -> do
    let i = eventIdFromPath path ^. _1
    (i,) <$> syncOne path
  pure $ M.fromList rdata

updateImpl :: (MonadKorrvigs m) => Event -> (ICalFile -> m ICalFile) -> m ()
updateImpl event f = do
  let path = event ^. eventFile
  ical <-
    liftIO (parseICalFile path)
      >>= throwEither (\err -> KMiscError $ "Failed to read \"" <> T.pack path <> "\" : " <> err)
  ncal <- f ical
  liftIO $ BSL.writeFile path $ renderICalFile ncal

updateMetadata :: (MonadKorrvigs m) => Event -> Map Text Value -> [Text] -> m ()
updateMetadata event upd rm =
  updateImpl event $ pure . (icEvent . _Just %~ doMtdt)
  where
    -- Title is a special case, it cannot be removed
    rmMtdt :: Text -> ICalEvent -> ICalEvent
    rmMtdt "categories" ievent = ievent & iceCategories .~ []
    rmMtdt key ievent = ievent & iceMtdt %~ M.delete (CI.mk key)
    updMtdt :: Text -> Value -> ICalEvent -> ICalEvent
    updMtdt "categories" val ievent = case fromJSON val of
      Success cats -> ievent & iceCategories .~ cats
      Error _ -> ievent
    updMtdt key val ievent | CI.mk key == mtdtName Title = case fromJSON val of
      Success txt -> ievent & iceSummary .~ txt
      Error _ -> ievent
    updMtdt key val ievent = ievent & iceMtdt . at (CI.mk key) ?~ val
    doMtdt :: ICalEvent -> ICalEvent
    doMtdt = foldr (.) id $ (uncurry updMtdt <$> M.toList upd) ++ (rmMtdt <$> rm)

updateParents :: (MonadKorrvigs m) => Event -> [Id] -> [Id] -> m ()
updateParents event toAdd toRm =
  updateImpl event $ pure . (icEvent . _Just . iceParents %~ updParents)
  where
    updParents = (toAdd ++) . filter (not . flip elem toRm)
