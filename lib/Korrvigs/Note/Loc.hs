module Korrvigs.Note.Loc
  ( SubLoc (..),
    subOffsets,
    CodeLoc (..),
    codeSub,
    codeOffset,
    CheckLoc (..),
    checkSub,
    checkOffset,
    AnyLoc (..),
    sub,
    getSub,
    setSub,
    code,
    getCode,
    setCode,
    check,
    getCheck,
    setCheck,
    renderSubLoc,
    renderCodeLoc,
    renderLoc,
    parseLoc,
    subPrefix,
  )
where

import Control.Lens
import Control.Monad
import Data.ByteString.Builder
import Data.Functor (($>))
import Data.List (isSuffixOf)
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

data CheckLoc = CheckLoc
  { _checkSub :: SubLoc,
    _checkOffset :: Int
  }
  deriving (Eq, Ord, Show, Read)

makeLenses ''CheckLoc

data AnyLoc
  = LocSub SubLoc
  | LocCode CodeLoc
  | LocCheck CheckLoc
  deriving (Eq, Ord, Show, Read)

subOff :: (Applicative f) => Int -> (Header -> f Header) -> [Block] -> f [Block]
subOff = elementOf (each . _Sub)

subPrefix :: SubLoc -> SubLoc -> Bool
subPrefix (SubLoc l1) (SubLoc l2) = l1 `isSuffixOf` l2

-- sub :: SubLoc -> Traversal' Document Header
sub :: (Applicative f) => SubLoc -> (Header -> f Header) -> Document -> f Document
sub (SubLoc []) = const pure
sub (SubLoc [off]) = docContent . subOff off
sub (SubLoc (off : offs)) = sub (SubLoc offs) . hdContent . subOff off

subContents :: (Applicative f) => SubLoc -> ([Block] -> f [Block]) -> Document -> f Document
subContents (SubLoc []) = docContent
subContents sb = sub sb . hdContent

getSub :: SubLoc -> Document -> Maybe Header
getSub loc doc = doc ^? sub loc

setSub :: SubLoc -> Document -> Header -> Document
setSub loc doc hd = doc & sub loc .~ hd

code :: (Applicative f) => CodeLoc -> (Text -> f Text) -> Document -> f Document
code (CodeLoc sb off) = subContents sb . elementOf (each . _CodeBlock . _2) off

getCode :: CodeLoc -> Document -> Maybe Text
getCode loc doc = doc ^? code loc

setCode :: CodeLoc -> Document -> Text -> Document
setCode loc doc cd = doc & code loc .~ cd

check :: (Applicative f) => CheckLoc -> (TaskStatus -> f TaskStatus) -> Document -> f Document
check (CheckLoc sb off) = subContents sb . elementOf (each . bkInlines . inlInlines . _Check) off

getCheck :: CheckLoc -> Document -> Maybe TaskStatus
getCheck loc doc = doc ^? check loc

setCheck :: CheckLoc -> Document -> TaskStatus -> Document
setCheck loc doc cb = doc & check loc .~ cb

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

buildCheckLoc :: CheckLoc -> Builder
buildCheckLoc loc =
  buildSubLoc (loc ^. checkSub . subOffsets) <> buildText ":x:" <> intDec (loc ^. checkOffset)

renderCheckLoc :: CheckLoc -> Text
renderCheckLoc = doRender . buildCheckLoc

renderLoc :: AnyLoc -> Text
renderLoc (LocSub loc) = renderSubLoc loc
renderLoc (LocCode loc) = renderCodeLoc loc
renderLoc (LocCheck loc) = renderCheckLoc loc

-- Parse AST location
subLocP :: (Stream s Identity Char) => Parsec s u SubLoc
subLocP = SubLoc . reverse <$> sepBy decimal (char '.')

locPrefixP :: (Stream s Identity Char) => Char -> Parsec s u ()
locPrefixP prefix = void $ char prefix >> char ':'

anyLocP :: (Stream s Identity Char) => SubLoc -> Parsec s u AnyLoc
anyLocP sb =
  locPrefixP 'c' *> (LocCode . CodeLoc sb <$> decimal)
    <|> locPrefixP 'x' *> (LocCheck . CheckLoc sb <$> decimal)

locP :: (Stream s Identity Char) => Parsec s u AnyLoc
locP = do
  sb <- subLocP
  eof $> LocSub sb <|> char ':' *> anyLocP sb

parseLoc :: Text -> Either Text AnyLoc
parseLoc loc = case parse locP "<loc>" loc of
  Left err -> Left . T.pack $ show err
  Right l -> Right l
