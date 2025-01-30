{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Korrvigs.Utils.DateParser where

import Control.Monad
import Control.Monad.Identity
import Data.Fixed
import Data.Functor (($>))
import Data.Time.Calendar
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Options.Applicative hiding (option, value)
import Text.Parsec hiding ((<|>))
import Text.Parsec.Number

rgDigit :: (Stream s Identity Char) => Int -> Int -> Int -> Parsec s u Int
rgDigit n mn mx = do
  v <- numberValue 10 <$> count n digit
  when (v < mn || v > mx) $ fail $ "Value expected in range [" <> show mn <> "-" <> show mx <> "]"
  pure v

yearP :: (Stream s Identity Char) => Parsec s u Year
yearP = numberValue 10 <$> count 4 digit

monthOfYearP :: (Stream s Identity Char) => Parsec s u MonthOfYear
monthOfYearP = rgDigit 2 1 12

dayOfMonthP :: (Stream s Identity Char) => Parsec s u DayOfMonth
dayOfMonthP = rgDigit 2 1 31

dayWithSepP :: (Stream s Identity Char) => Parsec s u a -> Parsec s u Day
dayWithSepP sep =
  fromGregorian
    <$> yearP
    <* sep
    <*> monthOfYearP
    <* sep
    <*> dayOfMonthP

dayP :: (Stream s Identity Char) => Parsec s u Day
dayP = dayWithSepP $ char '-'

timeOfDayP :: (Stream s Identity Char) => Parsec s u TimeOfDay
timeOfDayP =
  TimeOfDay
    <$> rgDigit 2 0 23
    <* char ':'
    <*> rgDigit 2 0 59
    <* char ':'
    <*> picoP

picoP :: (Stream s Identity Char) => Parsec s u Pico
picoP = do
  s' <- rgDigit 2 0 61
  let s = fromIntegral s' :: Integer
  d <- option 0 $ char '.' *> decimal
  let sz =
        if d == 0
          then 11
          else floor (logBase 10 $ fromIntegral d :: Double) :: Integer
  let dec =
        if sz >= 12
          then d `div` (10 ^ (sz - 11))
          else d * (10 ^ (11 - sz))
  let pow = (^) :: Integer -> Int -> Integer
  let value = (s * pow 10 12) + dec :: Integer
  pure $ MkFixed value

localTimeP :: (Stream s Identity Char) => Parsec s u LocalTime
localTimeP =
  LocalTime
    <$> dayP
    <* char 'T'
    <*> timeOfDayP

timezoneP :: (Stream s Identity Char) => Parsec s u TimeZone
timezoneP = do
  sgn <- oneOf "+-"
  h <- rgDigit 2 0 24
  void $ char ':'
  m <- rgDigit 2 0 59
  let dir = if sgn == '+' then 1 else -1
  pure $ TimeZone (dir * (h * 60 + m)) False ""

zonedTimeP :: (Stream s Identity Char) => Parsec s u ZonedTime
zonedTimeP = ZonedTime <$> localTimeP <*> timezoneP

zonedTimeZP :: (Stream s Identity Char) => Parsec s u ZonedTime
zonedTimeZP =
  ZonedTime
    <$> localTimeP
    <*> (try (char 'Z' $> TimeZone 0 False "") <|> timezoneP)

newtype DayParserResult a = DayParserResult {extractResult :: Either String a}
  deriving (Functor, Applicative, Monad)

instance MonadFail DayParserResult where
  fail = DayParserResult . Left

dayParser :: (ISO8601 t) => ReadM t
dayParser =
  eitherReader $
    extractResult . iso8601ParseM
