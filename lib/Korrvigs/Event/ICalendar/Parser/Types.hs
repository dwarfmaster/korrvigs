module Korrvigs.Event.ICalendar.Parser.Types where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Functor
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Word
import Korrvigs.Event.ICalendar.Parser.Basic
import Network.URI
import Text.Parsec

binaryP :: (Monad m, Stream s m Word8) => ParsecT s u m BS.ByteString
binaryP = B64.decodeBase64Lenient . BS.pack <$> ((++) <$> (mconcat <$> many bChar4) <*> bCharEnd)
  where
    bChar :: CharSpec
    bChar =
      Any
        [ alphaChar,
          digitChar,
          ExactByte 0x2B,
          ExactByte 0x2F
        ]
    bCharP = runCharSpecP bChar
    bChar4 = mconcat <$> replicateM 4 bCharP
    eqP = charP '=' $> [0x3D]
    bCharEnd =
      mconcat
        <$> ( sequence [bCharP, bCharP, eqP, eqP]
                <|> sequence [bCharP, bCharP, bCharP, eqP]
                <|> mempty
            )

boolP :: (Monad m, Stream s m Word8) => ParsecT s u m Bool
boolP = stringP "TRUE" $> True <|> stringP "FALSE" $> False

dateP :: (Monad m, Stream s m Word8) => ParsecT s u m Day
dateP = fromGregorian <$> digitsP 4 <*> digitsP 2 <*> digitsP 2

timeP :: (Monad m, Stream s m Word8) => ParsecT s u m (Bool, TimeOfDay)
timeP = do
  time <- TimeOfDay <$> digitsP 2 <*> digitsP 2 <*> digitsP 2
  isUTC <- try (charP 'Z' $> True) <|> pure False
  pure (isUTC, time)

-- The boolean indicates if the timezone is UTC or unspecified
dateTimeP :: (Monad m, Stream s m Word8) => ParsecT s u m (Bool, LocalTime)
dateTimeP = do
  date <- dateP
  charP 'T'
  (isUTC, time) <- timeP
  pure (isUTC, LocalTime date time)

signP :: (Monad m, Stream s m Word8) => ParsecT s u m Bool
signP =
  charP '+' $> True
    <|> charP '-' $> False
    <|> pure True

durWeekP :: (Monad m, Stream s m Word8) => ParsecT s u m Integer
durWeekP = do
  weeks <- numberP
  charP 'W'
  pure $ weeks * weekSecs
  where
    weekSecs :: Integer
    weekSecs = 7 * 24 * 3600

optSpec :: (Monad m, Stream s m Word8) => Char -> Integer -> ParsecT s u m Integer
optSpec spec secs = maybe 0 (* secs) <$> optionMaybe (numberP <* charP spec)

durGregP :: (Monad m, Stream s m Word8) => ParsecT s u m Integer
durGregP = do
  days <- optSpec 'D' daySecs
  optional $ charP 'T'
  hours <- optSpec 'H' hourSecs
  minutes <- optSpec 'M' minuteSecs
  seconds <- optSpec 'S' 1
  pure $ days + hours + minutes + seconds
  where
    daySecs :: Integer
    daySecs = 24 * hourSecs
    hourSecs :: Integer
    hourSecs = 60 * minuteSecs
    minuteSecs :: Integer
    minuteSecs = 60

-- The boolean indicates if this is a positive (True) or negative difftime
durationP :: (Monad m, Stream s m Word8) => ParsecT s u m NominalDiffTime
durationP = do
  sign <- signP
  charP 'P'
  secs <- durWeekP <|> durGregP
  pure $ fromInteger $ (if sign then 1 else -1) * secs

floatP :: (Monad m, Stream s m Word8) => ParsecT s u m Float
floatP = do
  sign <- signP
  integral :: Int <- numberP
  decimal :: Maybe Int <- optionMaybe (charP '.' >> numberP)
  let intF :: Float = fromIntegral integral
  let decF :: Float = maybe 0 fromIntegral decimal
  let e :: Int = ceiling (logBase 10 decF)
  let absolute = intF + decF / (10.0 ** fromIntegral e)
  pure $ (if sign then 1 else -1) * absolute

integerP :: (Monad m, Stream s m Word8) => ParsecT s u m Int
integerP = do
  sign <- signP
  absolute <- numberP
  pure $ (if sign then 1 else -1) * absolute

periodP :: (Monad m, Stream s m Word8) => ParsecT s u m (LocalTime, LocalTime)
periodP = do
  start <- snd <$> dateTimeP
  charP '/'
  end <- try (snd <$> dateTimeP) <|> (flip addLocalTime start <$> durationP)
  pure (start, end)

textChar :: CharSpec
textChar =
  Any
    [ wsp,
      ExactByte 0x21,
      ByteRange 0x23 0x2B,
      ByteRange 0x2D 0x39,
      ByteRange 0x3C 0x5B,
      ByteRange 0x5D 0x7E,
      nonUsAscii,
      ExactByte 0x3A,
      ExactByte 0x22
    ]

escapedChar :: (Monad m, Stream s m Word8) => ParsecT s u m Char
escapedChar = charP '\\' >> (c '\\' <|> c ';' <|> c ',' <|> charP 'n' $> '\n' <|> charP 'N' $> '\n')
  where
    c x = charP x $> x

textP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
textP = T.pack <$> many (escapedChar <|> charSpecP textChar)

uriP :: (Monad m, Stream s m Word8) => ParsecT s u m URI
uriP = maybe mzero pure . parseURI =<< many (charSpecP valueChar)

-- UTC offset in seconds
utcOffsetP :: (Monad m, Stream s m Word8) => ParsecT s u m Int
utcOffsetP = do
  sign <- charP '+' $> 1 <|> charP '-' $> -1
  hours <- digitsP 2
  minutes <- digitsP 2
  seconds <- fromMaybe 0 <$> optionMaybe (digitsP 2)
  pure $ sign * (hours * 3600 + minutes * 60 + seconds)
