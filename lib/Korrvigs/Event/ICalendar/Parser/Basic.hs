module Korrvigs.Event.ICalendar.Parser.Basic where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as B8
import Data.Functor
import Data.Word
import Text.Parsec

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

digitChar :: CharSpec
digitChar = ByteRange 0x30 0x39

alphaChar :: CharSpec
alphaChar = Any [ByteRange 0x41 0x5A, ByteRange 0x61 0x7A]

nameChar :: CharSpec
nameChar = Any [digitChar, ExactByte 0x2D, alphaChar]

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

stringP :: (Monad m, Stream s m Word8) => String -> ParsecT s u m ()
stringP = mapM_ charP

digitP :: (Num a, Read a, Monad m, Stream s m Word8) => ParsecT s u m a
digitP = charSpecP digitChar <&> read . (: [])

digitsP :: (Num a, Read a, Monad m, Stream s m Word8) => Int -> ParsecT s u m a
digitsP 0 = pure 0
digitsP n = do
  msd <- digitsP (n - 1)
  lsd <- digitP
  pure $ 10 * msd + lsd

numberP :: (Num a, Read a, Monad m, Stream s m Word8) => ParsecT s u m a
numberP = foldl (\n d -> 10 * n + d) 0 <$> many1 digitP
