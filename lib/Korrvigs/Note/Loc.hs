module Korrvigs.Note.Loc
  ( SubLoc (..),
    subOffsets,
    CodeLoc (..),
    codeSub,
    codeOffset,
    EmbedLoc (..),
    embedSub,
    embedOffset,
    DeepEmbedLoc (..),
    deepEmbed,
    CheckLoc (..),
    checkSub,
    checkOffset,
    TaskLoc (..),
    taskSub,
    AnyLoc (..),
    extractSubLoc,
    subLvl,
    sub,
    subs,
    getSub,
    setSub,
    codeFull,
    code,
    getCode,
    setCode,
    _embed,
    check,
    getCheck,
    setCheck,
    task,
    getTask,
    setTask,
    renderSubLoc,
    renderCodeLoc,
    renderLoc,
    parseLoc,
    subPrefix,
    renderDeepEmbedLoc,
    parseDeepEmbedLoc,
    renderEmbeddedLoc,
    parseEmbeddedLoc,
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
import Korrvigs.Entry.Ident
import Korrvigs.Metadata.Task
import Korrvigs.Note.AST
import Korrvigs.Utils.Lens
import Text.Parsec
import Text.Parsec.Number

-- AST location
newtype SubLoc = SubLoc
  { _subOffsets :: [Int] -- Offsets are in reversed order
  }
  deriving (Eq, Ord, Show, Read)

data CodeLoc = CodeLoc
  { _codeSub :: SubLoc,
    _codeOffset :: Int
  }
  deriving (Eq, Ord, Show, Read)

data EmbedLoc = EmbedLoc
  { _embedSub :: SubLoc,
    _embedOffset :: Int
  }
  deriving (Eq, Ord, Show, Read)

newtype DeepEmbedLoc = DeepEmbedLoc
  { _deepEmbed :: [EmbedLoc]
  }
  deriving (Eq, Ord, Show, Read)

data CheckLoc = CheckLoc
  { _checkSub :: SubLoc,
    _checkOffset :: Int
  }
  deriving (Eq, Ord, Show, Read)

newtype TaskLoc = TaskLoc
  { _taskSub :: SubLoc
  }
  deriving (Eq, Ord, Show, Read)

makeLenses ''SubLoc
makeLenses ''CodeLoc
makeLenses ''EmbedLoc
makeLenses ''DeepEmbedLoc
makeLenses ''CheckLoc
makeLenses ''TaskLoc

data AnyLoc
  = LocSub SubLoc
  | LocCode CodeLoc
  | LocCheck CheckLoc
  | LocTask TaskLoc
  deriving (Eq, Ord, Show, Read)

extractSubLoc :: AnyLoc -> SubLoc
extractSubLoc (LocSub loc) = loc
extractSubLoc (LocCode loc) = loc ^. codeSub
extractSubLoc (LocCheck loc) = loc ^. checkSub
extractSubLoc (LocTask loc) = loc ^. taskSub

subOff :: (Applicative f) => Int -> (Header -> f Header) -> [Block] -> f [Block]
subOff = elementOf (each . _Sub)

subPrefix :: SubLoc -> SubLoc -> Bool
subPrefix (SubLoc l1) (SubLoc l2) = l1 `isSuffixOf` l2

-- sub :: SubLoc -> Traversal' Document Header
sub :: (Applicative f) => SubLoc -> (Header -> f Header) -> Document -> f Document
sub (SubLoc []) = const pure
sub (SubLoc [off]) = docContent . subOff off
sub (SubLoc (off : offs)) = sub (SubLoc offs) . hdContent . subOff off

subLvl :: SubLoc -> Int
subLvl (SubLoc lst) = length lst

subsOff :: (Applicative f) => Int -> ([Block] -> f [Block]) -> [Block] -> f [Block]
subsOff i f bks = expandAt offset f bks
  where
    findOffset :: Int -> Int -> [Block] -> Int
    findOffset _ off [] = off
    findOffset j off (Sub _ : _) | i == j = off
    findOffset j off (Sub _ : bs) = findOffset (j + 1) (off + 1) bs
    findOffset j off (_ : bs) = findOffset j (off + 1) bs
    offset = findOffset 0 0 bks

-- subs: Traversal giving the list of headers at a loc and all the followings
subs :: (Applicative f) => SubLoc -> ([Block] -> f [Block]) -> Document -> f Document
subs (SubLoc []) = const pure
subs (SubLoc [off]) = docContent . subsOff off
subs (SubLoc (off : offs)) = sub (SubLoc offs) . hdContent . subsOff off

subContents :: (Applicative f) => SubLoc -> ([Block] -> f [Block]) -> Document -> f Document
subContents (SubLoc []) = docContent
subContents sb = sub sb . hdContent

getSub :: SubLoc -> Document -> Maybe Header
getSub loc doc = doc ^? sub loc

setSub :: SubLoc -> Document -> Header -> Document
setSub loc doc hd = doc & sub loc .~ hd

codeFull :: (Applicative f) => CodeLoc -> ((Attr, Text) -> f (Attr, Text)) -> Document -> f Document
codeFull (CodeLoc sb off) = subContents sb . elementOf (each . bkBlocks . _CodeBlock) off

code :: (Applicative f) => CodeLoc -> (Text -> f Text) -> Document -> f Document
code loc = codeFull loc . _2

getCode :: CodeLoc -> Document -> Maybe Text
getCode loc doc = doc ^? code loc

setCode :: CodeLoc -> Document -> Text -> Document
setCode loc doc cd = doc & code loc .~ cd

_embed :: (Semigroup (f Block), Applicative f) => EmbedLoc -> (Either Text Id -> f (Either Text Id)) -> Document -> f Document
_embed loc = subContents (loc ^. embedSub) . elementOf (each . bkBlocks . (_Embed <> (_EmbedHeader . _1))) (loc ^. embedOffset)

check :: (Applicative f) => CheckLoc -> (TaskStatus -> f TaskStatus) -> Document -> f Document
check (CheckLoc sb off) = subContents sb . elementOf (each . bkInlines . inlInlines . _Check) off

getCheck :: CheckLoc -> Document -> Maybe TaskStatus
getCheck loc doc = doc ^? check loc

setCheck :: CheckLoc -> Document -> TaskStatus -> Document
setCheck loc doc cb = doc & check loc .~ cb

task :: (Applicative f) => TaskLoc -> (Task -> f Task) -> Document -> f Document
task loc = sub (loc ^. taskSub) . hdTask . _Just

getTask :: TaskLoc -> Document -> Maybe Task
getTask loc doc = doc ^? task loc

setTask :: TaskLoc -> Document -> Task -> Document
setTask loc doc tsk = doc & task loc .~ tsk

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

buildTaskLoc :: TaskLoc -> Builder
buildTaskLoc loc =
  buildSubLoc (loc ^. taskSub . subOffsets) <> buildText ":t:"

renderTaskLoc :: TaskLoc -> Text
renderTaskLoc = doRender . buildTaskLoc

renderLoc :: AnyLoc -> Text
renderLoc (LocSub loc) = renderSubLoc loc
renderLoc (LocCode loc) = renderCodeLoc loc
renderLoc (LocCheck loc) = renderCheckLoc loc
renderLoc (LocTask loc) = renderTaskLoc loc

buildDeepEmbedLoc :: [EmbedLoc] -> Builder
buildDeepEmbedLoc [] = mempty
buildDeepEmbedLoc [l] =
  buildSubLoc (l ^. embedSub . subOffsets) <> ":" <> intDec (l ^. embedOffset)
buildDeepEmbedLoc (l : ls) =
  buildSubLoc (l ^. embedSub . subOffsets)
    <> ":"
    <> intDec (l ^. embedOffset)
    <> "|"
    <> buildDeepEmbedLoc ls

renderDeepEmbedLoc :: DeepEmbedLoc -> Text
renderDeepEmbedLoc = doRender . buildDeepEmbedLoc . view deepEmbed

renderEmbeddedLoc :: (DeepEmbedLoc, SubLoc) -> Text
renderEmbeddedLoc (dp, loc) =
  doRender $ buildDeepEmbedLoc (dp ^. deepEmbed) <> "," <> buildSubLoc (loc ^. subOffsets)

-- Parse AST location
subLocP :: (Stream s Identity Char) => Parsec s u SubLoc
subLocP = SubLoc . reverse <$> sepBy decimal (char '.')

locPrefixP :: (Stream s Identity Char) => Char -> Parsec s u ()
locPrefixP prefix = void $ char prefix >> char ':'

anyLocP :: (Stream s Identity Char) => SubLoc -> Parsec s u AnyLoc
anyLocP sb =
  locPrefixP 'c' *> (LocCode . CodeLoc sb <$> decimal)
    <|> locPrefixP 'x' *> (LocCheck . CheckLoc sb <$> decimal)
    <|> (locPrefixP 't' $> (LocTask . TaskLoc) sb)

locP :: (Stream s Identity Char) => Parsec s u AnyLoc
locP = do
  sb <- subLocP
  eof $> LocSub sb <|> char ':' *> anyLocP sb

parseLoc :: Text -> Either Text AnyLoc
parseLoc loc = case parse locP "<loc>" loc of
  Left err -> Left . T.pack $ show err
  Right l -> Right l

embedLocP :: (Stream s Identity Char) => Parsec s u EmbedLoc
embedLocP = do
  sb <- subLocP
  void $ char ':'
  off <- decimal
  pure $ EmbedLoc sb off

deepEmbedLocP :: (Stream s Identity Char) => Parsec s u DeepEmbedLoc
deepEmbedLocP = DeepEmbedLoc <$> sepBy embedLocP (char '|')

parseDeepEmbedLoc :: Text -> Either Text DeepEmbedLoc
parseDeepEmbedLoc loc = case parse deepEmbedLocP "<embedloc>" loc of
  Left err -> Left . T.pack $ show err
  Right l -> Right l

embeddedLocP :: (Stream s Identity Char) => Parsec s u (DeepEmbedLoc, AnyLoc)
embeddedLocP = do
  d <- deepEmbedLocP
  void $ char ','
  l <- locP
  pure (d, l)

parseEmbeddedLoc :: Text -> Either Text (DeepEmbedLoc, AnyLoc)
parseEmbeddedLoc loc = case parse embeddedLocP "<embedloc>" loc of
  Left err -> Left . T.pack $ show err
  Right l -> Right l
