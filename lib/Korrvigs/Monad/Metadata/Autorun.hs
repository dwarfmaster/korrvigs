module Korrvigs.Monad.Metadata.Autorun
  ( RunPeriod (..),
    periodLength,
    periodAlign,
    AutoRunnableTarget (..),
    AutoRunnable (..),
    autoTarget,
    autoPeriod,
    autoLastRun,
    autoRunTime,
    targetsToRun,
    targetsToRunTimed,
  )
where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar hiding (periodLength)
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Syndicate.SQL
import Korrvigs.Utils
import Korrvigs.Utils.JSON
import Opaleye hiding (lower)
import Text.Parsec
import Text.Parsec.Number

data RunPeriod = RunPeriod
  { _periodLength :: CalendarDiffDays,
    _periodAlign :: Maybe DayOfWeek
  }
  deriving (Show)

makeLenses ''RunPeriod

periodParser :: (Stream s Identity Char) => Parsec s u RunPeriod
periodParser = do
  n <- decimal
  c <- oneOf "dwmy"
  let period =
        CalendarDiffDays
          (if c == 'y' then n * 12 else if c == 'm' then n else 0)
          (if c == 'w' then n * 7 else if c == 'd' then n else 0)
  align <- optionMaybe $ do
    void spaces
    day <- many1 lower
    case day of
      "mon" -> pure Monday
      "tue" -> pure Tuesday
      "wed" -> pure Wednesday
      "thu" -> pure Thursday
      "fri" -> pure Friday
      "sat" -> pure Saturday
      "sun" -> pure Sunday
      _ -> fail $ "Invalid day: " <> day
  pure $ RunPeriod period align

parsePeriod :: Text -> Either Text RunPeriod
parsePeriod txt = case parse periodParser "<period>" txt of
  Left err -> Left . T.pack $ show err
  Right p -> Right p

newtype AutoRunnableTarget = AutoSyn Syndicate
  deriving (Show)

data AutoRunnable = AutoRunnable
  { _autoTarget :: AutoRunnableTarget,
    _autoPeriod :: RunPeriod,
    _autoLastRun :: Maybe UTCTime,
    _autoRunTime :: Maybe Int
  }
  deriving (Show)

makePrisms ''AutoRunnableTarget
makeLenses ''AutoRunnable

listSyndicateTargets :: (MonadKorrvigs m) => m [AutoRunnable]
listSyndicateTargets = do
  sql <- rSelect $ do
    entry <- selectTable entriesTable
    syn <- selectTable syndicatesTable
    where_ $ entry ^. sqlEntryId .== (syn ^. sqlSynId)
    autorun <- baseSelectTextMtdt AutoRun $ entry ^. sqlEntryId
    lastRun <- selectTextMtdt RunDate $ entry ^. sqlEntryId
    runTime <- selectMtdt RunTime $ entry ^. sqlEntryId
    pure (entry, syn, autorun, lastRun, runTime)
  parsed <- forM sql $ \(entryR, synR, autorunT, lastRunT, runTimeV) -> fromMaybeT [] $ do
    let syn = entryFromRow SyndicateD entryR (synFromRow synR)
    period <- hoistEither $ parsePeriod autorunT
    date <- forM lastRunT $ iso8601ParseM . T.unpack
    time <- forM runTimeV $ hoistMaybe . fromJSONM
    pure $
      singleton $
        AutoRunnable
          { _autoTarget = AutoSyn syn,
            _autoPeriod = period,
            _autoLastRun = date,
            _autoRunTime = time
          }
  pure $ mconcat parsed

listTargets :: (MonadKorrvigs m) => m [AutoRunnable]
listTargets = listSyndicateTargets

shouldRunTarget :: Day -> AutoRunnable -> Bool
shouldRunTarget today tgt = case tgt ^? autoLastRun . _Just . to utctDay of
  Nothing -> True
  Just runDay ->
    let diff = diffGregorianDurationClip today runDay
     in ( (cdMonths len > 0 && cdMonths diff >= cdMonths len)
            || (cdDays len > 0 && 28 * cdMonths diff + cdDays diff >= cdDays len)
        )
          && maybe True (ensureDayInWindow diff runDay) (tgt ^. autoPeriod . periodAlign)
  where
    len = tgt ^. autoPeriod . periodLength
    ensureDayInWindow diff runDay day =
      cdMonths diff > 0
        || cdDays diff >= 7
        || (dayOfWeek today > day && dayOfWeek runDay <= day)
        || (dayOfWeek today > day && dayOfWeek runDay > dayOfWeek today)

compareTargets :: AutoRunnable -> AutoRunnable -> Ordering
compareTargets tgt1 tgt2 =
  compare
    (hasRun1, period1, tgt1 ^. autoLastRun, Down $ tgt1 ^. autoRunTime)
    (hasRun2, period2, tgt2 ^. autoLastRun, Down $ tgt2 ^. autoRunTime)
  where
    hasRun1 = isJust $ tgt1 ^. autoLastRun
    hasRun2 = isJust $ tgt2 ^. autoLastRun
    toDays diff = 28 * cdMonths diff + cdDays diff
    period1 = tgt1 ^. autoPeriod . periodLength . to toDays
    period2 = tgt2 ^. autoPeriod . periodLength . to toDays

targetsToRun :: (MonadKorrvigs m) => m [AutoRunnable]
targetsToRun = do
  today <- liftIO $ utctDay <$> getCurrentTime
  sortBy compareTargets . filter (shouldRunTarget today) <$> listTargets

targetsToRunTimed :: (MonadKorrvigs m) => Int -> m [AutoRunnable]
targetsToRunTimed time = do
  targets <- targetsToRun
  let nrt s tgt = let ns = s + fromMaybe 0 (tgt ^. autoRunTime) in (ns, (ns, tgt))
  let targetsWithTime = snd $ mapAccumL nrt 0 targets
  pure $ snd <$> takeWhile ((< time) . fst) targetsWithTime
