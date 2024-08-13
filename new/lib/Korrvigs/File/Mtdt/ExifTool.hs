{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.File.Mtdt.ExifTool (extract) where

import Control.Applicative (asum, (<|>))
import Control.Monad
import Control.Monad.Identity
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.List.NonEmpty
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Network.Mime
import System.FilePath
import System.Process
import Text.Parsec hiding ((<|>))
import Text.Parsec.Number (decimal)
import Text.Read (readMaybe)

type Parser = Parsec ByteString ()

spaces1 :: Parser ()
spaces1 = space >> spaces

exifLine :: Parser (Text, Text, Text)
exifLine = do
  void $ char '['
  category <- many1 $ noneOf "]"
  void $ char ']'
  spaces1
  name <- many1 $ noneOf ":"
  void $ string ": "
  value <- many1 $ noneOf "\n"
  pure (T.pack category, T.strip (T.pack name), T.pack value)

exifOutput :: Parser [(Text, Text, Text)]
exifOutput = sepEndBy exifLine newline <* eof

type Mapping = Map Text (NonEmpty (Text, Text))

extract :: FilePath -> MimeType -> IO (Map Text Value)
extract path _ = do
  let exifTool = proc "exiftool" ["-G", "-a", "-s", path]
  (_, Just out, _, _) <- createProcess exifTool {std_out = CreatePipe}
  content <- BS.hGetContents out
  let result = parse exifOutput (takeFileName path) content
  case result of
    Left err -> do
      putStrLn $ "Failed to parse exif output: " <> show err
      pure M.empty
    Right mtdt -> do
      let mappings = M.unionsWith (<>) $ (\(category, name, value) -> M.singleton name ((category, value) :| [])) <$> mtdt
      pure $ mconcat $ ($ mappings) <$> [getTitle, getPageCount, getDate, getDimensions]

seqLookup :: (Ord a) => Map a b -> [a] -> Maybe b
seqLookup _ [] = Nothing
seqLookup mp (key : keys) = M.lookup key mp <|> seqLookup mp keys

getTitle :: Mapping -> Map Text Value
getTitle mtdt = case seqLookup mtdt ["Title", "BookName", "UpdatedTitle"] of
  Just ((_, title) :| _) -> M.singleton "title" $ toJSON title
  Nothing -> M.empty

getPageCount :: Mapping -> Map Text Value
getPageCount mtdt = case seqLookup mtdt ["PageCount"] of
  Just ((_, cnt) :| _) -> case readMaybe (T.unpack cnt) :: Maybe Int of
    Just c -> M.singleton "pages" $ toJSON c
    Nothing -> M.empty
  Nothing -> M.empty

getDate :: Mapping -> Map Text Value
getDate mtdt = case seqLookup mtdt ["DateTimeOriginal", "CreateDate", "ModifyDate", "FileModifyDate"] of
  Just ((_, dt) :| _) ->
    let formats = ["%Y:%m:%d %T%Ez", "%Y:%m:%d %T"]
     in let results = (\f -> parseTimeM True defaultTimeLocale f (T.unpack dt)) <$> formats :: [Maybe ZonedTime]
         in case asum results of
              Just zonedTime -> M.singleton "date" $ toJSON $ iso8601Show zonedTime
              Nothing -> M.empty
  Nothing -> M.empty

getDimensions :: Mapping -> Map Text Value
getDimensions mtdt = case (M.lookup "ImageWidth" mtdt, M.lookup "ImageHeight" mtdt) of
  (Just ((_, width) :| _), Just ((_, height) :| _)) ->
    case (readMaybe (T.unpack width), readMaybe (T.unpack height)) :: (Maybe Int, Maybe Int) of
      (Just w, Just h) -> M.fromList [("width", toJSON w), ("height", toJSON h)]
      _ -> M.empty
  _ -> case M.lookup "ImageSize" mtdt of
    Just ((_, size) :| _) -> case parse parseSize "" size of
      Left _ -> M.empty
      Right (w, h) -> M.fromList [("width", toJSON w), ("height", toJSON h)]
    Nothing -> M.empty
  where
    parseSize :: (Stream s Identity Char) => Parsec s u (Int, Int)
    parseSize = do
      w <- decimal
      void $ char 'x'
      h <- decimal
      eof
      pure (w, h)