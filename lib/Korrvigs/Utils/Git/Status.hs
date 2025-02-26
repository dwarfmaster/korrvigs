{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Utils.Git.Status where

import Control.DeepSeq (NFData, deepseq)
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Maybe
import Data.Text hiding (isAscii)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Word (Word8)
import GHC.Generics (Generic)
import Korrvigs.Monad
import System.IO
import System.Process
import Text.Parsec
import Text.Parsec.Number (decimal, numberValue)

data Short
  = Unchanged
  | Updated
  | TypeChanged
  | Added
  | Deleted
  | Renamed
  | Copied
  deriving (Eq, Ord, Show, Generic, NFData)

data Score
  = CopiedScore Int
  | MovedScore Int
  deriving (Eq, Ord, Show, Generic, NFData)

data FileChanged = FileChanged
  { _changeIndex :: Short,
    _changeTree :: Short,
    _changeHEADObject :: Text,
    _changeIndexObject :: Text,
    _changePath :: FilePath
  }
  deriving (Eq, Ord, Show, Generic, NFData)

data FileMoved = FileMoved
  { _moveIndex :: Short,
    _moveTree :: Short,
    _moveHEADObject :: Text,
    _moveIndexObject :: Text,
    _moveScore :: Score,
    _moveSource :: FilePath,
    _moveTarget :: FilePath
  }
  deriving (Eq, Ord, Show, Generic, NFData)

data FileStatus
  = StatusChanged FileChanged
  | StatusMoved FileMoved
  | StatusUnknown FilePath
  | StatusIgnored FilePath
  deriving (Eq, Ord, Show, Generic, NFData)

makeLenses ''FileChanged
makeLenses ''FileMoved

gitStatus :: FilePath -> IO (Either Text [FileStatus])
gitStatus repo = do
  devNull <- openFile "/dev/null" WriteMode
  let gstatus =
        (proc "git" ["status", ".", "--porcelain=v2", "--no-renames", "--untracked-files=all"])
          { std_out = CreatePipe,
            std_err = UseHandle devNull,
            cwd = Just repo
          }
  (_, mstdout, _, prc) <- createProcess gstatus
  contents <- maybe (pure LBS.empty) LBS.hGetContents mstdout
  let r = case runParser allStatusP () "git status" contents of
        Left err -> Left $ T.pack $ show err
        Right v -> Right v
  deepseq r $ void $ waitForProcess prc
  hClose devNull
  pure r

gitStatusKorr :: (MonadKorrvigs m) => m [FileStatus]
gitStatusKorr = root >>= liftIO . gitStatus >>= throwEither KMiscError

-- Parser
type Parser = Parsec ByteString ()

sep :: Parser ()
sep = void $ char ' '

statusP :: Parser (Maybe FileStatus)
statusP =
  oneOf "12?!#" >>= \case
    '1' -> Just . StatusChanged <$> (sep *> changedP)
    '2' -> Just . StatusMoved <$> (sep *> movedP)
    '?' -> Just . StatusUnknown <$> (sep *> pathP)
    '!' -> Just . StatusIgnored <$> (sep *> pathP)
    _ -> many (noneOf "\n\r") >> pure Nothing

allStatusP :: Parser [FileStatus]
allStatusP = (catMaybes <$> sepEndBy statusP newline) <* eof

shortP :: Parser Short
shortP =
  oneOf ".MTADRC" <&> \case
    'M' -> Updated
    'T' -> TypeChanged
    'A' -> Added
    'D' -> Deleted
    'R' -> Renamed
    'C' -> Copied
    _ -> Unchanged

modeP :: Parser ()
modeP = replicateM_ 6 octDigit

objectP :: Parser Text
objectP = T.pack <$> replicateM 40 alphaNum

subP :: Parser ()
subP = replicateM_ 4 anyChar

changedP :: Parser FileChanged
changedP = do
  indexStatus <- shortP
  treeStatus <- shortP
  sep
  subP
  sep
  modeP
  sep
  modeP
  sep
  modeP
  sep
  headObj <- objectP
  sep
  treeObj <- objectP
  sep
  FileChanged indexStatus treeStatus headObj treeObj <$> pathP

scoreP :: Parser Score
scoreP =
  oneOf "RC" >>= \case
    'R' -> MovedScore <$> decimal
    _ -> CopiedScore <$> decimal

movedP :: Parser FileMoved
movedP = do
  indexStatus <- shortP
  treeStatus <- shortP
  sep
  subP
  sep
  modeP
  sep
  modeP
  sep
  modeP
  sep
  headObj <- objectP
  sep
  treeObj <- objectP
  sep
  score <- scoreP
  sep
  source <- pathP
  void $ oneOf "\t\0"
  FileMoved indexStatus treeStatus headObj treeObj score source <$> pathP

pathP :: Parser FilePath
pathP =
  between (char '"') (char '"') (dec <$> pathUtf8P)
    <|> many1 (noneOf "\n\t\r\0")
  where
    dec :: ByteString -> FilePath
    dec = LT.unpack . LEnc.decodeUtf8

pathUtf8P :: Parser ByteString
pathUtf8P = LBS.pack <$> many ((char '\\' >> parseCharCode) <|> asciiChar)
  where
    asciiChar :: Parser Word8
    asciiChar = fromInteger . toInteger . ord <$> satisfy (\c -> c /= '"' && isAscii c)
    parseCharCode :: Parser Word8
    parseCharCode = numberValue 8 <$> replicateM 3 digit
