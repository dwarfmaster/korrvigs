module Korrvigs.File.Mtdt.ExifTool (extract) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Network.Mime
import System.FilePath
import System.Process
import Text.Parsec hiding ((<|>))

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
      let mappings = M.unionsWith (<>) $ (\(category, name, value) -> M.singleton name [(category, value)]) <$> mtdt
      pure $ mconcat $ ($ mappings) <$> [getTitle]

seqLookup :: (Ord a) => Map a b -> [a] -> Maybe b
seqLookup _ [] = Nothing
seqLookup mp (key : keys) = M.lookup key mp <|> seqLookup mp keys

getTitle :: Map Text [(Text, Text)] -> Map Text Value
getTitle mtdt = case seqLookup mtdt ["Title", "BookName", "UpdatedTitle"] of
  Just ((_, title) : _) -> M.singleton "title" $ toJSON title
  Just _ -> M.empty
  Nothing -> M.empty
