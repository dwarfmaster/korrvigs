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
    targetsRun,
    displayTarget,
  )
where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Profunctor.Product.Default
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Base64
import qualified Data.Text.IO as TIO
import Data.Time.Calendar hiding (periodLength)
import Data.Time.Clock
import Data.Time.Format.ISO8601
import qualified Korrvigs.Calendar.DAV as Cal
import Korrvigs.Calendar.SQL
import qualified Korrvigs.Compute.Run as Comp
import Korrvigs.Compute.SQL
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Monad.Computation (getComputation)
import qualified Korrvigs.Syndicate.Run as Syn
import Korrvigs.Syndicate.SQL
import Korrvigs.Utils
import Korrvigs.Utils.JSON
import Opaleye hiding (lower)
import qualified Opaleye as O
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

data AutoRunnableTarget
  = AutoSyn Syndicate
  | AutoCal Calendar
  | AutoCode Id Text
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

listComputationTargets :: (MonadKorrvigs m) => m [AutoRunnable]
listComputationTargets = do
  sql <- rSelect $ do
    comp <- selectTable computationsTable
    where_ $ O.not $ isNull $ comp ^. sqlCompAutorun
    let autorun = fromNullable (sqlStrictText "") $ comp ^. sqlCompAutorun
    entry <- nameFor $ comp ^. sqlCompEntry
    pure $ comp & sqlCompEntry .~ entry & sqlCompAutorun .~ autorun
  fmap mconcat $ forM sql $ \comp -> fromMaybeT [] $ do
    period <- hoistEither $ parsePeriod $ comp ^. sqlCompAutorun
    pure $
      singleton $
        AutoRunnable
          { _autoTarget = AutoCode (comp ^. sqlCompEntry) (comp ^. sqlCompName),
            _autoPeriod = period,
            _autoLastRun = comp ^. sqlCompLastRun,
            _autoRunTime = comp ^. sqlCompRunTime
          }

listEntryTargets ::
  (MonadKorrvigs m, Default FromFields sql row, Default Unpackspec sql sql) =>
  Table sql sql ->
  (sql -> Field SqlInt4) ->
  (a -> KindData) ->
  (row -> Entry -> a) ->
  (a -> AutoRunnableTarget) ->
  m [AutoRunnable]
listEntryTargets tbl getId constr fromRow mkTarget = do
  sql <- rSelect $ do
    entry <- selectTable entriesTable
    kd <- selectTable tbl
    where_ $ entry ^. sqlEntryId .== getId kd
    autorun <- baseSelectTextMtdt AutoRun $ entry ^. sqlEntryId
    lastRun <- selectTextMtdt RunDate $ entry ^. sqlEntryId
    runTime <- selectMtdt RunTime $ entry ^. sqlEntryId
    pure (entry, kd, autorun, lastRun, runTime)
  parsed <- forM sql $ \(entryR, kdR, autorunT, lastRunT, runTimeV) -> fromMaybeT [] $ do
    let kd = entryFromRow constr entryR (fromRow kdR)
    period <- hoistEither $ parsePeriod autorunT
    date <- forM lastRunT $ iso8601ParseM . T.unpack
    time <- forM runTimeV $ hoistMaybe . fromJSONM
    pure $
      singleton $
        AutoRunnable
          { _autoTarget = mkTarget kd,
            _autoPeriod = period,
            _autoLastRun = date,
            _autoRunTime = time
          }
  pure $ mconcat parsed

listSyndicateTargets :: (MonadKorrvigs m) => m [AutoRunnable]
listSyndicateTargets =
  listEntryTargets syndicatesTable (view sqlSynId) SyndicateD synFromRow AutoSyn

listCalendarTargets :: (MonadKorrvigs m) => m [AutoRunnable]
listCalendarTargets =
  listEntryTargets calendarsTable (view sqlCalId) CalendarD calFromRow AutoCal

listTargets :: (MonadKorrvigs m) => m [AutoRunnable]
listTargets =
  mconcat
    <$> sequence
      [ listSyndicateTargets,
        listCalendarTargets,
        listComputationTargets
      ]

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

targetRun :: (MonadKorrvigs m) => AutoRunnableTarget -> m ()
targetRun (AutoSyn syn) = void $ runResourceT $ Syn.run syn
targetRun (AutoCal cal) = fromMaybeT () $ do
  davCreds <- hoistLift $ getCredential "caldav"
  pwdEnc <- hoistMaybe $ M.lookup (cal ^. calServer) davCreds
  let pwd = T.strip $ decodeBase64Lenient pwdEnc
  void $ lift $ Cal.syncCalendar (liftIO . putStrLn . T.unpack) cal pwd
targetRun (AutoCode i code) = fromMaybeT () $ do
  comp <- hoistLift $ getComputation i code
  r <- lift $ Comp.runForce comp
  case r of
    Left err ->
      liftIO $ TIO.putStrLn $ "Computation " <> unId i <> "#" <> code <> " failed: " <> err
    Right _ -> pure ()

displayTarget :: AutoRunnableTarget -> Text
displayTarget (AutoSyn syn) = "syn:" <> unId (syn ^. synEntry . entryName)
displayTarget (AutoCal cal) = "cal:" <> unId (cal ^. calEntry . entryName)
displayTarget (AutoCode i code) = "code:" <> unId i <> "#" <> code

targetsRun :: (MonadKorrvigs m) => Int -> m ()
targetsRun time = do
  targets <- targetsToRunTimed time
  forM_ targets $ \auto -> do
    let tgt = auto ^. autoTarget
    liftIO $ putStrLn $ "Running " <> T.unpack (displayTarget tgt)
    targetRun tgt
