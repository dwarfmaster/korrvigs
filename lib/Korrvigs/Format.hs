module Korrvigs.Format (Formatter, FormatSpec, run, parse, entrySpec) where

import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.Reader
import Data.Functor
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
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Utils.Lens
import Text.Builder (Builder)
import qualified Text.Builder as Bld
import Text.Parsec hiding (parse)

newtype FormatSpec a = FmtSpec {unFmtSpec :: Map Text (a -> [Builder])}

liftSpec :: ((b -> Const [b] b) -> a -> Const [b] a) -> FormatSpec b -> FormatSpec a
liftSpec get = FmtSpec . fmap (\f -> toMonoid (: []) get >=> f) . unFmtSpec

fromLens :: ((Builder -> Const [Builder] Builder) -> a -> Const [Builder] a) -> (a -> [Builder])
fromLens = toMonoid (: [])

instance Semigroup (FormatSpec a) where
  (FmtSpec mp1) <> (FmtSpec mp2) =
    FmtSpec $ M.unionWith (\f1 f2 x -> f1 x ++ f2 x) mp1 mp2

instance Monoid (FormatSpec a) where
  mempty = FmtSpec M.empty

fromList :: [(Text, a -> [Builder])] -> FormatSpec a
fromList = FmtSpec . M.fromList

singleton :: Text -> (a -> [Builder]) -> FormatSpec a
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

daySpec :: FormatSpec Day
daySpec =
  fromList
    [ ("year", fromLens $ to toGregorian . _1 . to (padded 4)),
      ("month", fromLens $ to toGregorian . _2 . to (padded 2)),
      ("monthLong", fromLens $ to toGregorian . _2 . to monthName . _1),
      ("monthShort", fromLens $ to toGregorian . _2 . to monthName . _2),
      ("day", fromLens $ to toGregorian . _3 . to (padded 2)),
      ("dayWeek", fromLens $ to dayOfWeek . to dayName . _1 . to Bld.decimal),
      ("dayWeekLong", fromLens $ to dayOfWeek . to dayName . _2),
      ("dayWeekShort", fromLens $ to dayOfWeek . to dayName . _3),
      ("dayIso", fromLens $ to iso8601Show . to Bld.string)
    ]

timeSpec :: FormatSpec TimeOfDay
timeSpec =
  fromList
    [ ("hour", fromLens $ to todHour . to (padded 2)),
      ("minute", fromLens $ to todMin . to (padded 2)),
      ("second", fromLens $ to todSec . to toRational . to flr . to (padded 2)),
      ("timeIso", fromLens $ to iso8601Show . to Bld.string)
    ]
  where
    flr :: Rational -> Int
    flr = floor

dateSpec :: FormatSpec ZonedTime
dateSpec =
  fromList
    [ ("timezone", fromLens $ to zonedTimeZone . to timeZoneMinutes . to displayTZ),
      ("dateIso", fromLens $ to iso8601Show . to Bld.string)
    ]
    <> liftSpec (to zonedTimeToLocalTime . to localDay) daySpec
    <> liftSpec (to zonedTimeToLocalTime . to localTimeOfDay) timeSpec

linkSpec :: FormatSpec Link
linkSpec =
  fromList
    [ ("protocol", fromLens $ linkProtocol . to Bld.text),
      ("ref", fromLens $ linkRef . to Bld.text),
      ("path", fromLens $ linkPath . to Bld.string)
    ]

noteSpec :: FormatSpec Note
noteSpec = singleton "path" $ fromLens $ notePath . to Bld.string

fileSpec :: FormatSpec File
fileSpec =
  fromList
    [ ("path", fromLens $ filePath . to Bld.string),
      ("status", fromLens $ fileStatus . to displayFileStatus . to Bld.text),
      ("mime", fromLens $ fileMime . to Enc.decodeUtf8 . to Bld.text)
    ]

-- TODO add specific metadata one the mecanism is here
entrySpec :: FormatSpec Entry
entrySpec =
  fromList
    [ ("name", fromLens $ name . to unId . to Bld.text),
      ("kind", fromLens $ kind . to displayKind . to Bld.text)
    ]
    <> liftSpec (date . _Just) dateSpec
    <> liftSpec _Link linkSpec
    <> liftSpec _Note noteSpec
    <> liftSpec _File fileSpec

newtype Formatter a = Fmt (ReaderT a Maybe Builder)

run :: Formatter a -> a -> Maybe Text
run (Fmt monad) = fmap Bld.run . runReaderT monad

type FmtM a = ReaderT a Maybe

type Parser a = Parsec Text (FormatSpec a)

parse :: FormatSpec a -> Text -> Either Text (Formatter a)
parse spec txt =
  case runParser formatP spec "" txt of
    Right fmt -> Right $ Fmt fmt
    Left err -> Left $ T.pack $ show err

formatP :: Parser a (FmtM a Builder)
formatP =
  eof $> pure mempty
    <|> fmtP
    <|> plainP

fmtSpecP :: Parser a (FmtM a Builder)
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
      blds <- asks dat
      case (blds, rep) of
        ([], Nothing) -> lift Nothing
        ([], Just r) -> pure $ Bld.string r
        _ -> pure . mconcat $ intersperse sp blds
    Nothing -> mzero

fmtP :: Parser a (FmtM a Builder)
fmtP = do
  f <- between (char '{') (char '}') fmtSpecP
  liftA2 (<>) f <$> formatP

plainP :: Parser a (FmtM a Builder)
plainP = do
  w <- many $ noneOf "{"
  fmap (Bld.string w <>) <$> formatP
