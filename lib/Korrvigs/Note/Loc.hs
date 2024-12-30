module Korrvigs.Note.Loc
  ( SubLoc (..),
    subOffsets,
    CodeLoc (..),
    codeSub,
    codeOffset,
    AnyLoc (..),
    sub,
    getSub,
    setSub,
    code,
    getCode,
    setCode,
    renderSubLoc,
    renderCodeLoc,
    renderLoc,
    parseLoc,
  )
where

import Control.Lens
import Control.Monad
import Data.ByteString.Builder
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Note.AST
import Text.Parsec
import Text.Parsec.Number

-- AST location
newtype SubLoc = SubLoc
  { _subOffsets :: [Int] -- Offsets are in reversed order
  }
  deriving (Eq, Ord, Show, Read)

makeLenses ''SubLoc

data CodeLoc = CodeLoc
  { _codeSub :: SubLoc,
    _codeOffset :: Int
  }
  deriving (Eq, Ord, Show, Read)

makeLenses ''CodeLoc

data AnyLoc
  = LocSub SubLoc
  | LocCode CodeLoc
  deriving (Eq, Ord, Show, Read)

subOff :: (Applicative f) => Int -> (Header -> f Header) -> [Block] -> f [Block]
subOff = elementOf (each . _Sub)

-- sub :: SubLoc -> Traversal' Document Header
sub :: (Applicative f) => SubLoc -> (Header -> f Header) -> Document -> f Document
sub (SubLoc []) = const pure
sub (SubLoc [off]) = docContent . subOff off
sub (SubLoc (off : offs)) = sub (SubLoc offs) . hdContent . subOff off

getSub :: SubLoc -> Document -> Maybe Header
getSub loc doc = doc ^? sub loc

setSub :: SubLoc -> Document -> Header -> Document
setSub loc doc hd = doc & sub loc .~ hd

code :: (Applicative f) => CodeLoc -> (Text -> f Text) -> Document -> f Document
code (CodeLoc sb off) = sub sb . hdContent . elementOf (each . _CodeBlock . _2) off

getCode :: CodeLoc -> Document -> Maybe Text
getCode loc doc = doc ^? code loc

setCode :: CodeLoc -> Document -> Text -> Document
setCode loc doc cd = doc & code loc .~ cd

-- Rendering AST locations
buildSubLoc :: [Int] -> Builder
buildSubLoc [] = mempty
buildSubLoc [i] = intDec i
buildSubLoc (i : is) = buildSubLoc is <> charUtf8 '.' <> intDec i

doRender :: Builder -> Text
doRender = LT.toStrict . LEnc.decodeASCII . toLazyByteString

renderSubLoc :: SubLoc -> Text
renderSubLoc = doRender . buildSubLoc . (^. subOffsets)

buildText :: Text -> Builder
buildText txt = mconcat $ txt ^.. each . to charUtf8

buildCodeLoc :: CodeLoc -> Builder
buildCodeLoc loc =
  buildSubLoc (loc ^. codeSub . subOffsets) <> buildText ":c:" <> intDec (loc ^. codeOffset)

renderCodeLoc :: CodeLoc -> Text
renderCodeLoc = doRender . buildCodeLoc

renderLoc :: AnyLoc -> Text
renderLoc (LocSub loc) = renderSubLoc loc
renderLoc (LocCode loc) = renderCodeLoc loc

-- Parse AST location
subLocP :: (Stream s Identity Char) => Parsec s u SubLoc
subLocP = SubLoc . reverse <$> sepBy decimal (char '.')

locPrefixP :: (Stream s Identity Char) => Char -> Parsec s u ()
locPrefixP prefix = void $ char prefix >> char ':'

anyLocP :: (Stream s Identity Char) => SubLoc -> Parsec s u AnyLoc
anyLocP sb =
  locPrefixP 'c' *> (LocCode . CodeLoc sb <$> decimal)

locP :: (Stream s Identity Char) => Parsec s u AnyLoc
locP = do
  sb <- subLocP
  eof $> LocSub sb <|> char ':' *> anyLocP sb

parseLoc :: Text -> Either Text AnyLoc
parseLoc loc = case parse locP "<loc>" loc of
  Left err -> Left . T.pack $ show err
  Right l -> Right l
