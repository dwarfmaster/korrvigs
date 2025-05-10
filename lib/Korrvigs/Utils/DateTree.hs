module Korrvigs.Utils.DateTree (DateTreeType, dtYear, dtMonth, dtDay, FileContent (..), storeFile, listFiles) where

-- Helper module to deal with files organised in date based trees

import Control.Lens
import Control.Monad (forM)
import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString, writeFile)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import System.Directory
import System.FilePath
import System.Posix.Files (createSymbolicLink, deviceID, getFileStatus)
import Text.Parsec
import Text.Parsec.Number
import Text.Printf
import Prelude hiding (writeFile)

data DateTreeType = DateTreeType
  { _dtYear :: Bool,
    _dtMonth :: Bool,
    _dtDay :: Bool
  }
  deriving (Eq, Ord, Show)

makeLenses ''DateTreeType

instance Default DateTreeType where
  def = DateTreeType False False False

getCurrentDay :: IO Day
getCurrentDay = localDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)

type DayData = (Year, MonthOfYear, DayOfMonth)

data FileContent
  = FileLazy ByteString
  | FileCopy FilePath
  | FileMove FilePath

storeFile ::
  (MonadIO m) =>
  FilePath ->
  DateTreeType ->
  Maybe Day ->
  Text ->
  FileContent ->
  m FilePath
storeFile root tp mday name content = liftIO $ do
  createDirectoryIfMissing True root
  day <- toGregorian <$> maybe getCurrentDay pure mday
  if tp ^. dtYear
    then storeFileYear root tp day name content
    else
      if tp ^. dtMonth
        then storeFileMonth root tp day name content
        else
          if tp ^. dtDay
            then storeFileDay root day name content
            else storeFilePlain root name content

storeFileYear :: FilePath -> DateTreeType -> DayData -> Text -> FileContent -> IO FilePath
storeFileYear root tp day name content = do
  let dir = joinPath [root, printf "%04d" $ day ^. _1]
  createDirectoryIfMissing False dir
  if tp ^. dtMonth
    then storeFileMonth dir tp day name content
    else
      if tp ^. dtDay
        then storeFileDay dir day name content
        else storeFilePlain dir name content

storeFileMonth :: FilePath -> DateTreeType -> DayData -> Text -> FileContent -> IO FilePath
storeFileMonth root tp day name content = do
  let dir = joinPath [root, printf "%02d" $ day ^. _2]
  createDirectoryIfMissing False dir
  if tp ^. dtDay
    then storeFileDay dir day name content
    else storeFilePlain dir name content

storeFileDay :: FilePath -> DayData -> Text -> FileContent -> IO FilePath
storeFileDay root day name content = do
  let dir = joinPath [root, printf "%02d" $ day ^. _3]
  createDirectoryIfMissing False dir
  storeFilePlain dir name content

copyLink :: FilePath -> FilePath -> IO ()
copyLink src tgt = do
  lnk <- getSymbolicLinkTarget src
  createSymbolicLink lnk tgt

renameIfPossible :: FilePath -> FilePath -> IO ()
renameIfPossible src tgt = do
  let dir1 = takeDirectory src
  stat1 <- getFileStatus dir1
  let dir2 = takeDirectory tgt
  stat2 <- getFileStatus dir2
  if deviceID stat1 == deviceID stat2
    then renameFile src tgt
    else do
      copyFile src tgt
      removeFile src

storeFilePlain :: FilePath -> Text -> FileContent -> IO FilePath
storeFilePlain root name content = do
  let path = joinPath [root, T.unpack name]
  case content of
    FileLazy lbs -> writeFile path lbs
    FileCopy file ->
      pathIsSymbolicLink file >>= \sym ->
        if sym
          then copyLink file path
          else copyFile file path
    FileMove file ->
      pathIsSymbolicLink file >>= \sym ->
        if sym
          then copyLink file path >> removeFile file
          else renameIfPossible file path
  pure path

type DateFile = (FilePath, Maybe Year, Maybe MonthOfYear, Maybe DayOfMonth)

parseInteger :: (Integral a) => String -> Maybe a
parseInteger str = case parse (number 10 digit) "directory" str of
  Right r -> Just r
  Left _ -> Nothing

listFiles ::
  (MonadIO m) =>
  FilePath ->
  DateTreeType ->
  m [DateFile]
listFiles root tp =
  liftIO $
    doesDirectoryExist root >>= \case
      False -> pure []
      True -> do
        if tp ^. dtYear
          then listFilesYear root tp
          else
            if tp ^. dtMonth
              then listFilesMonth root tp Nothing
              else
                if tp ^. dtDay
                  then listFilesDay root Nothing Nothing
                  else listFilesPlain root Nothing Nothing Nothing

listFilesYear :: FilePath -> DateTreeType -> IO [DateFile]
listFilesYear root tp = do
  content <- getDirectoryContents root
  res <- forM content $ \file ->
    parseInteger file & \case
      Nothing -> pure []
      Just year ->
        let dir = joinPath [root, file]
         in if tp ^. dtMonth
              then listFilesMonth dir tp (Just year)
              else
                if tp ^. dtDay
                  then listFilesDay dir (Just year) Nothing
                  else listFilesPlain dir (Just year) Nothing Nothing
  pure $ mconcat res

listFilesMonth :: FilePath -> DateTreeType -> Maybe Year -> IO [DateFile]
listFilesMonth root tp year = do
  content <- getDirectoryContents root
  res <- forM content $ \file ->
    parseInteger file & \case
      Nothing -> pure []
      Just month ->
        let dir = joinPath [root, file]
         in if tp ^. dtDay
              then listFilesDay dir year (Just month)
              else listFilesPlain dir year (Just month) Nothing
  pure $ mconcat res

listFilesDay :: FilePath -> Maybe Year -> Maybe MonthOfYear -> IO [DateFile]
listFilesDay root year month = do
  content <- getDirectoryContents root
  res <- forM content $ \file ->
    parseInteger file & \case
      Nothing -> pure []
      Just day -> listFilesPlain (joinPath [root, file]) year month $ Just day
  pure $ mconcat res

listFilesPlain :: FilePath -> Maybe Year -> Maybe MonthOfYear -> Maybe DayOfMonth -> IO [DateFile]
listFilesPlain root year month day =
  fmap (\f -> (joinPath [root, f], year, month, day)) . filter (\p -> p /= "." && p /= "..")
    <$> getDirectoryContents root
