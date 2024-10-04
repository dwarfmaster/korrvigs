{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Event.ICalendar.Parser where

import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as B8
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Korrvigs.Event.ICalendar.Defs
import Text.Parsec

parseICalFile :: FilePath -> IO (Either Text ICalFile)
parseICalFile path = do
  content <- BSL.readFile path
  let r = runParserT parser () path $ BSL.unpack content
  case runIdentity r of
    Left err ->
      pure . Left . T.pack $ "\"" <> show err <> "\" at " <> show (errorPos err)
    Right v -> pure $ Right v
  where
    parser :: ParsecT [Word8] () Identity ICalFile
    parser = icalFileP <* eof

-- ByteRanges
data CharSpec
  = ByteRange Word8 Word8 -- Inclusive word range
  | ExactByte Word8
  | Any [CharSpec]
  | Seq [CharSpec]

utf8Tail :: CharSpec
utf8Tail = ByteRange 0x80 0xBF

utf8_2 :: CharSpec
utf8_2 = Seq [ByteRange 0xC2 0xDF, utf8Tail]

utf8_3 :: CharSpec
utf8_3 =
  Any
    [ Seq [ExactByte 0xE0, ByteRange 0xA0 0xBF, utf8Tail],
      Seq [ByteRange 0xE1 0xEC, utf8Tail, utf8Tail],
      Seq [ExactByte 0xED, ByteRange 0x80 0x9F, utf8Tail],
      Seq [ByteRange 0xEE 0xEF, utf8Tail, utf8Tail]
    ]

utf8_4 :: CharSpec
utf8_4 =
  Any
    [ Seq [ExactByte 0xF0, ByteRange 0x90 0xBF, utf8Tail, utf8Tail],
      Seq [ByteRange 0xF1 0xF3, utf8Tail, utf8Tail, utf8Tail],
      Seq [ExactByte 0xF4, ByteRange 0x80 0x8F, utf8Tail, utf8Tail]
    ]

nonUsAscii :: CharSpec
nonUsAscii = Any [utf8_2, utf8_3, utf8_4]

wsp :: CharSpec
wsp = Any [ExactByte 0x20, ExactByte 0x09]

valueChar :: CharSpec
valueChar = Any [wsp, ByteRange 0x21 0x7E, nonUsAscii]

safeChar :: CharSpec
safeChar = Any [wsp, ExactByte 0x21, ByteRange 0x23 0x2B, ByteRange 0x2D 0x39, ByteRange 0x3C 0x7E, nonUsAscii]

qSafeChar :: CharSpec
qSafeChar = Any [wsp, ExactByte 0x21, ByteRange 0x23 0x7E, nonUsAscii]

nameChar :: CharSpec
nameChar = Any [ByteRange 0x30 0x39, ExactByte 0x2D, ByteRange 0x41 0x5A, ByteRange 0x61 0x7A]

getWord8 :: (Monad m, Stream s m Word8) => (Word8 -> Bool) -> ParsecT s u m Word8
getWord8 p = tokenPrim showW nextPos doP
  where
    showW c = "'" ++ show c ++ "'"
    nextPos pos c _ = if c == 0x0A then incSourceLine (setSourceColumn pos 1) 1 else incSourceColumn pos 1
    doP c = if p c then Just c else Nothing

-- Ignore sequences of CRLF followed by space
nextWord8 :: (Monad m, Stream s m Word8) => (Word8 -> Bool) -> ParsecT s u m Word8
nextWord8 p = many (try seqP) >> getWord8 p
  where
    seqP = (try (getWord8 (== 0x0D)) <|> pure 0x0D) >> getWord8 (== 0x0A) >> getWord8 (== 0x20)

runCharSpecP :: (Monad m, Stream s m Word8) => CharSpec -> ParsecT s u m [Word8]
runCharSpecP (ByteRange low high) = (: []) <$> nextWord8 (\c -> c >= low && c <= high)
runCharSpecP (ExactByte v) = (: []) <$> nextWord8 (== v)
runCharSpecP (Seq specs) = mconcat <$> mapM runCharSpecP specs
runCharSpecP (Any []) = mzero
runCharSpecP (Any [spec]) = runCharSpecP spec
runCharSpecP (Any (spec : specs)) = try (runCharSpecP spec) <|> runCharSpecP (Any specs)

charSpecP :: (Monad m, Stream s m Word8) => CharSpec -> ParsecT s u m Char
charSpecP spec = do
  bytes <- runCharSpecP spec
  let mc = B8.decode $ BS.pack bytes
  case mc of
    Nothing -> mzero
    Just (c, _) -> pure c

byteSeqP :: (Monad m, Stream s m Word8) => [Word8] -> ParsecT s u m ()
byteSeqP = void . charSpecP . Seq . fmap ExactByte

charP :: (Monad m, Stream s m Word8) => Char -> ParsecT s u m ()
charP = byteSeqP . BS.unpack . B8.fromChar

-- Follows RFC5545:
--   https://datatracker.ietf.org/doc/html/rfc5545

quotedStringP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
quotedStringP = do
  charP '"'
  chars <- many $ charSpecP qSafeChar
  charP '"'
  pure $ T.pack chars

valueP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
valueP = T.pack <$> many (charSpecP valueChar)

paramTextP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
paramTextP = T.pack <$> many (charSpecP safeChar)

paramValueP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
paramValueP = paramTextP <|> quotedStringP

paramNameP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
paramNameP = T.pack <$> many1 (charSpecP nameChar)

paramP :: (Monad m, Stream s m Word8) => ParsecT s u m (Text, [Text])
paramP = do
  name <- paramNameP
  charP '='
  values <- sepBy1 paramValueP (charP ',')
  pure (name, values)

nameP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
nameP = paramNameP

crlfP :: (Monad m, Stream s m Word8) => ParsecT s u m ()
crlfP = try (byteSeqP [0x0D, 0x0A]) <|> byteSeqP [0x0A]

contentLineP :: (Monad m, Stream s m Word8) => ParsecT s u m (Text, ICalValue)
contentLineP = do
  name <- nameP
  params <- many $ charP ';' >> paramP
  charP ':'
  val <- valueP
  crlfP
  pure (name, ICValue (M.fromList params) val)

icalFileP :: (Monad m, Stream s m Word8) => ParsecT s u m ICalFile
icalFileP = do
  (name, val) <- contentLineP
  unless (name == "BEGIN" && val ^. icValue == "VCALENDAR") mzero
  let start = ICFile "" [] M.empty
  ical <- icalFileRecP start
  pure $ ical & icGroups %~ reverse

icalFileRecP :: (Monad m, Stream s m Word8) => ICalFile -> ParsecT s u m ICalFile
icalFileRecP ical = do
  (name, val) <- contentLineP
  case name of
    "BEGIN" -> do
      group <- icalGroupP $ val ^. icValue
      icalFileRecP $ ical & icGroups %~ (group :)
    "END" -> do
      unless (val ^. icValue == "VCALENDAR") mzero
      pure ical
    "VERSION" -> icalFileRecP (ical & icVersion .~ val ^. icValue)
    _ -> icalFileRecP (ical & icOther . at name ?~ val)

icalGroupP :: (Monad m, Stream s m Word8) => Text -> ParsecT s u m ICalGroup
icalGroupP tp = do
  let ictype = case tp of
        "VEVENT" -> VEVENT
        "VTIMEZONE" -> VTIMEZONE
        _ -> VOTHER tp
  let start = ICGroup ictype M.empty []
  icgroup <- icalGroupRecP tp start
  pure $ icgroup & icSubGroups %~ reverse

icalGroupRecP :: (Monad m, Stream s m Word8) => Text -> ICalGroup -> ParsecT s u m ICalGroup
icalGroupRecP tp icgroup = do
  (name, val) <- contentLineP
  case name of
    "BEGIN" -> do
      group <- icalGroupP $ val ^. icValue
      icalGroupRecP tp $ icgroup & icSubGroups %~ (group :)
    "END" -> do
      unless (val ^. icValue == tp) mzero
      pure icgroup
    _ -> icalGroupRecP tp $ icgroup & icValues . at name ?~ val
