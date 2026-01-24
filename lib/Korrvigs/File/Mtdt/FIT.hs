module Korrvigs.File.Mtdt.FIT where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.FIT
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Korrvigs.File.Sync
import Korrvigs.Geometry
import Korrvigs.Metadata
import Korrvigs.Utils
import Linear.V2
import Network.Mime
import System.FilePath

extractPoints :: FilePath -> IO [Point]
extractPoints path = do
  runResourceT $
    runConduit $
      sourceFile path .| fitInterpret (Just path) .| extractPoint .| sinkList
  where
    extractPoint = whileJust_ await $ \msg -> fromMaybeT () $ do
      rec <- hoistMaybe $ msg ^? _FitMsgRecord
      lat <- hoistMaybe $ rec ^. fitRecordPosLat
      lon <- hoistMaybe $ rec ^. fitRecordPosLon
      lift $ yield $ V2 lon lat

extract :: FilePath -> MimeType -> IO (FileMetadata -> FileMetadata)
extract path _
  | takeExtension path == ".fit" = do
      msgs <- runResourceT $ runConduit $ sourceFile path .| fitInterpret (Just path) .| sinkList
      let dev = msgs ^? each . _FitMsgFileId . fitFileIdProductName . _Just
      let sport = msgs ^? each . _FitMsgSession . fitSessionSport . _Just
      let subsport = msgs ^? each . _FitMsgSession . fitSessionSubSport . _Just
      let act = join $ mkActivity <$> sport <*> subsport
      let dates = msgs ^.. each . _FitMsgRecord . fitRecordTimestamp . _Just
      let start = minimum dates
      print start
      let end = maximum dates
      let duration = diffUTCTime end start
      let titleTime = T.pack $ formatTime defaultTimeLocale "(%d %B %Y)" start
      let title = (<> (" activity " <> titleTime)) . activityName <$> act
      let lats = msgs ^.. each . _FitMsgRecord . fitRecordPosLat . _Just
      let lons = msgs ^.. each . _FitMsgRecord . fitRecordPosLon . _Just
      let geo = GeoPath $ zipWith V2 lons lats
      tz <- getCurrentTimeZone
      pure $
        foldr
          (.)
          id
          [ annoted . at (mtdtSqlName Device) ?~ toJSON dev,
            exGeo ?~ geo,
            maybe id ((annoted . at (mtdtSqlName ActivityMtdt) ?~) . toJSON) act,
            exDate ?~ utcToZonedTime tz start,
            exDuration ?~ CalendarDiffTime 0 duration,
            maybe id (exTitle ?~) title
          ]
  | otherwise = pure id
  where
    mkActivity :: FITSport -> FITSubSport -> Maybe Activity
    mkActivity FITCycling FITSubRoad = Just ActRoadCycling
    mkActivity FITCycling FITSubMountain = Just ActFatBiking
    mkActivity FITCycling FITSubTrackCycling = Just ActTrackBiking
    mkActivity FITWalking _ = Just ActHiking
    mkActivity FITSnowShoeing _ = Just ActSnowShoeing
    mkActivity _ _ = Nothing
