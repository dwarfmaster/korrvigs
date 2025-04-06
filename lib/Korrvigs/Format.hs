module Korrvigs.Format (Formatter, FormatSpec, run, parse, entrySpec) where

import Control.Applicative (empty)
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON)
import Data.Functor
import Data.ISBN
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time.Calendar
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Collections
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Utils.Lens
import Text.Builder (Builder)
import qualified Text.Builder as Bld
import Text.Parsec hiding (parse)

newtype FormatSpec m a = FmtSpec {unFmtSpec :: Map Text (a -> m [Builder])}

class Buildable m a where
  build :: a -> m [Builder]

instance (Applicative m) => Buildable m Text where
  build = pure . (: []) . Bld.text

instance (Applicative m) => Buildable m Int where
  build = pure . (: []) . Bld.decimal

instance (Applicative m) => Buildable m Builder where
  build = pure . (: [])

instance (Applicative m) => Buildable m ZonedTime where
  build = pure . (: []) . Bld.string . iso8601Show

instance (Applicative m) => Buildable m ISBN where
  build = pure . (: []) . Bld.text . renderISBN

instance (Monad m, Buildable m a) => Buildable m [a] where
  build l = mconcat <$> mapM build l

liftSpec :: (Monad m) => ((b -> Const [b] b) -> a -> Const [b] a) -> FormatSpec m b -> FormatSpec m a
liftSpec get =
  FmtSpec . fmap (\f -> fmap mconcat . mapM f . toMonoid (: []) get) . unFmtSpec

fromLens :: (Monad m, Buildable m b) => ((b -> Const [b] b) -> a -> Const [b] a) -> (a -> m [Builder])
fromLens get = build . toMonoid (: []) get

instance (Applicative m) => Semigroup (FormatSpec m a) where
  (FmtSpec mp1) <> (FmtSpec mp2) =
    FmtSpec $ M.unionWith (\f1 f2 x -> liftA2 (++) (f1 x) (f2 x)) mp1 mp2

instance (Applicative m) => Monoid (FormatSpec m a) where
  mempty = FmtSpec M.empty

fromList :: [(Text, a -> m [Builder])] -> FormatSpec m a
fromList = FmtSpec . M.fromList

singleton :: Text -> (a -> m [Builder]) -> FormatSpec m a
singleton key = FmtSpec . M.singleton key

-- Assumes v is positive
padded :: (Integral n) => Int -> n -> Builder
padded pad v = mconcat (replicate (pad - size) "0") <> Bld.decimal v
  where
    flr :: Double -> Int
    flr = floor
    size :: Int
    size = 1 + flr (logBase 10 $ fromIntegral v)

displayTZ :: Int -> Builder
displayTZ minutes = sign <> padded 2 hours <> padded 2 rest
  where
    sign = if minutes >= 0 then "+" else "-"
    (hours, rest) = divMod minutes 60

monthName :: Int -> (Builder, Builder)
monthName 1 = ("January", "Jan")
monthName 2 = ("February", "Feb")
monthName 3 = ("March", "Mar")
monthName 4 = ("April", "Apr")
monthName 5 = ("May", "May")
monthName 6 = ("June", "Jun")
monthName 7 = ("July", "Jul")
monthName 8 = ("August", "Aug")
monthName 9 = ("September", "Sep")
monthName 10 = ("October", "Oct")
monthName 11 = ("November", "Nov")
monthName 12 = ("December", "Dec")
monthName _ = error "Invalid month number"

dayName :: DayOfWeek -> (Int, Builder, Builder)
dayName Monday = (1, "Monday", "Mon")
dayName Tuesday = (2, "Tuesday", "Tue")
dayName Wednesday = (3, "Wednesday", "Wed")
dayName Thursday = (4, "Thursday", "Thu")
dayName Friday = (5, "Friday", "Fri")
dayName Saturday = (6, "Saturday", "Sat")
dayName Sunday = (7, "Sunday", "Sun")

daySpec :: (Monad m) => FormatSpec m Day
daySpec =
  fromList
    [ ("year", fromLens $ to toGregorian . _1 . to (padded 4)),
      ("month", fromLens $ to toGregorian . _2 . to (padded 2)),
      ("monthLong", fromLens $ to toGregorian . _2 . to monthName . _1),
      ("monthShort", fromLens $ to toGregorian . _2 . to monthName . _2),
      ("day", fromLens $ to toGregorian . _3 . to (padded 2)),
      ("dayWeek", fromLens $ to dayOfWeek . to dayName . _1),
      ("dayWeekLong", fromLens $ to dayOfWeek . to dayName . _2),
      ("dayWeekShort", fromLens $ to dayOfWeek . to dayName . _3),
      ("dayIso", fromLens $ to iso8601Show . to T.pack)
    ]

timeSpec :: (Monad m) => FormatSpec m TimeOfDay
timeSpec =
  fromList
    [ ("hour", fromLens $ to todHour . to (padded 2)),
      ("minute", fromLens $ to todMin . to (padded 2)),
      ("second", fromLens $ to todSec . to toRational . to flr . to (padded 2)),
      ("timeIso", fromLens $ to iso8601Show . to T.pack)
    ]
  where
    flr :: Rational -> Int
    flr = floor

dateSpec :: (Monad m) => FormatSpec m ZonedTime
dateSpec =
  fromList
    [ ("timezone", fromLens $ to zonedTimeZone . to timeZoneMinutes . to displayTZ),
      ("dateIso", fromLens $ to iso8601Show . to T.pack)
    ]
    <> liftSpec (to zonedTimeToLocalTime . to localDay) daySpec
    <> liftSpec (to zonedTimeToLocalTime . to localTimeOfDay) timeSpec

linkSpec :: (MonadKorrvigs m) => FormatSpec m Link
linkSpec =
  fromList
    [ ("protocol", fromLens linkProtocol),
      ("ref", fromLens linkRef),
      ("path", fromLens $ linkPath . to T.pack)
    ]

noteSpec :: (MonadKorrvigs m) => FormatSpec m Note
noteSpec = singleton "path" $ fromLens $ notePath . to T.pack

fileSpec :: (MonadKorrvigs m) => FormatSpec m File
fileSpec =
  fromList
    [ ("path", fromLens $ filePath . to T.pack),
      ("status", fromLens $ fileStatus . to displayFileStatus),
      ("mime", fromLens $ fileMime . to Enc.decodeUtf8)
    ]

eventSpec :: (MonadKorrvigs m) => FormatSpec m Event
eventSpec =
  fromList
    [ ("path", fromLens $ eventFile . to T.pack),
      ("uid", fromLens eventUid),
      ("calendar", fromLens $ eventCalendar . to unId)
    ]

calSpec :: (MonadKorrvigs m) => FormatSpec m Calendar
calSpec =
  singleton "path" (fmap ((: []) . Bld.string) . Cal.calendarPath)
    <> fromList
      [ ("calname", fromLens calName),
        ("server", fromLens calServer),
        ("user", fromLens calUser)
      ]

kindDataSpec :: (MonadKorrvigs m) => Kind -> FormatSpec m Entry
kindDataSpec Link = liftSpec _Link linkSpec
kindDataSpec Note = liftSpec _Note noteSpec
kindDataSpec File = liftSpec _File fileSpec
kindDataSpec Event = liftSpec _Event eventSpec
kindDataSpec Calendar = liftSpec _Calendar calSpec

renderMtdt :: (MonadKorrvigs m, ExtraMetadata mtdt, Buildable m (MtdtType mtdt), FromJSON (MtdtType mtdt)) => mtdt -> Entry -> m [Builder]
renderMtdt mtdt entry =
  rSelectMtdt mtdt (sqlId $ entry ^. name) >>= \case
    Nothing -> pure []
    Just v -> build v

mtdtSpec :: (MonadKorrvigs m, ExtraMetadata mtdt, Buildable m (MtdtType mtdt), FromJSON (MtdtType mtdt)) => mtdt -> FormatSpec m Entry
mtdtSpec mtdt = FmtSpec $ M.singleton (mtdtSqlName mtdt) (renderMtdt mtdt)

entrySpec :: (MonadKorrvigs m) => FormatSpec m Entry
entrySpec =
  fromList
    [ ("name", fromLens $ name . to unId),
      ("kind", fromLens $ kind . to displayKind)
    ]
    <> liftSpec (date . _Just) dateSpec
    <> foldMap kindDataSpec [minBound .. maxBound]
    <> mtdtSpec Title
    <> mtdtSpec Language
    <> mtdtSpec Authors
    <> mtdtSpec Pages
    <> mtdtSpec Height
    <> mtdtSpec Width
    <> mtdtSpec Gallery
    <> mtdtSpec TaskMtdt
    <> mtdtSpec TaskDeadline
    <> mtdtSpec TaskScheduled
    <> mtdtSpec TaskStarted
    <> mtdtSpec TaskFinished
    <> mtdtSpec Favourite
    <> mtdtSpec MiscCollection
    <> mtdtSpec GalleryCollection
    <> mtdtSpec TaskSet
    <> mtdtSpec Abstract
    <> mtdtSpec BibtexKey
    <> mtdtSpec DOI
    <> mtdtSpec ISBNMtdt
    <> mtdtSpec ISSN
    <> mtdtSpec Url
    <> mtdtSpec Feed
    <> mtdtSpec Publisher
    <> mtdtSpec InCollection
    <> mtdtSpec Institution
    <> mtdtSpec License

type FmtM m a = ReaderT a (MaybeT m)

newtype Formatter m a = Fmt (FmtM m a Builder)

run :: (Monad m) => Formatter m a -> a -> m (Maybe Text)
run (Fmt monad) = fmap (fmap Bld.run) . runMaybeT . runReaderT monad

type Parser m a = Parsec Text (FormatSpec m a)

parse :: (Monad m) => FormatSpec m a -> Text -> Either Text (Formatter m a)
parse spec txt =
  case runParser formatP spec "" txt of
    Right fmt -> Right $ Fmt fmt
    Left err -> Left $ T.pack $ show err

formatP :: (Monad m) => Parser m a (FmtM m a Builder)
formatP =
  eof $> pure mempty
    <|> fmtP
    <|> plainP

fmtSpecP :: (Monad m) => Parser m a (FmtM m a Builder)
fmtSpecP = do
  spec <- T.pack <$> many (noneOf ":}")
  sep <- option Nothing $ try $ do
    void $ char ':'
    s <- many $ noneOf ":}"
    pure $ Just s
  rep <- option Nothing $ try $ do
    void $ char ':'
    r <- many $ noneOf "}"
    pure $ Just r
  specData <- getState
  case M.lookup spec (unFmtSpec specData) of
    Just dat -> pure $ do
      let sp = Bld.string $ fromMaybe " " sep
      blds <- asks dat >>= lift . lift
      case (blds, rep) of
        ([], Nothing) -> lift empty
        ([], Just r) -> pure $ Bld.string r
        _ -> pure . mconcat $ intersperse sp blds
    Nothing -> mzero

fmtP :: (Monad m) => Parser m a (FmtM m a Builder)
fmtP = do
  f <- between (char '{') (char '}') fmtSpecP
  liftA2 (<>) f <$> formatP

plainP :: (Monad m) => Parser m a (FmtM m a Builder)
plainP = do
  w <- many $ noneOf "{"
  fmap (Bld.string w <>) <$> formatP
