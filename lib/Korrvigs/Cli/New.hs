module Korrvigs.Cli.New (newEntryOptions) where

import Control.Arrow (first)
import Control.Monad
import Data.Aeson
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Functor.Identity
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Korrvigs.Entry.Ident
import Korrvigs.Entry.New
import Korrvigs.Utils.DateParser (dayParser)
import Options.Applicative
import qualified Text.Parsec as P

parseCollection :: Text -> Maybe (Id, Text)
parseCollection col = case T.split (== '#') col of
  [i, nm] -> Just (MkId i, nm)
  _ -> Nothing

newEntryOptions :: Parser NewEntry
newEntryOptions =
  NewEntry . fmap MkId
    <$> many (option str $ metavar "ID" <> long "parent" <> help "Parent(s) for the added entry, can be repeated")
    <*> optional (option dayParser $ metavar "DATE" <> long "date" <> help "Date for the entry, in format year-month-day")
    <*> optional (option str $ metavar "TITLE" <> long "title")
    <*> optional (option str $ metavar "LANG" <> long "lang" <> help "Either fr or en, the language the entry will be interpreted as")
    <*> (M.fromList <$> many (option mtdtParser $ long "mtdt" <> help "Pairs in the form key=json of metadata to add to the entry"))
    <*> (catMaybes <$> many (parseCollection <$> option str (long "collection" <> metavar "COL" <> help "Collections to add the entry to, in the form id#colname")))
    <*> pure []

mtdtParser :: ReadM (CI Text, Value)
mtdtParser = eitherReader $ \s -> case P.runParser mtdtP () "<mtdt>" s of
  Left err -> Left $ show err
  Right v -> Right $ first CI.mk v

mtdtP :: (P.Stream s Identity Char) => P.Parsec s u (Text, Value)
mtdtP = do
  key <- T.pack <$> P.many1 (P.alphaNum <|> P.oneOf "-_")
  void $ P.char '='
  val <- T.pack <$> P.manyTill P.anyChar P.eof
  case eitherDecodeStrict (Enc.encodeUtf8 val) of
    Left err -> fail err
    Right v -> pure (key, v)
