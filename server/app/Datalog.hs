module Datalog where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Text.ParserCombinators.Parsec

-- import Text.ParserCombinators.Parsec.Char

data Entry = MkEntry
  { uuid :: UUID,
    sub :: Maybe Text,
    query :: Maybe Text
  }
  deriving (Show, Eq)

data Atom
  = Entry Entry
  | Int Integer
  | Number Double
  | Str Text
  | Var Text
  deriving (Show, Eq)

data Class
  = EntryT
  | NumberT
  | IntT
  | StrT
  deriving (Show, Eq)

data Predicate = MkPred
  { name :: Text,
    args :: [Atom]
  }
  deriving (Show, Eq)

data Constraint
  = Equality Atom Atom
  | Difference Atom Atom
  | Negated Predicate
  deriving (Show, Eq)

data Rule = MkRule
  { head :: Predicate,
    preds :: [Predicate],
    constraints :: [Constraint]
  }
  deriving (Show, Eq)

uuidP :: GenParser Char st UUID
uuidP = do
  uuid_str <- count 36 anyToken
  let uuid_may = UUID.fromString uuid_str
  case uuid_may of
    Just uuid -> return uuid
    Nothing -> fail "Couldn't parse uuid"

entryP :: GenParser Char st Entry
entryP =
  between (try $ char '\'') (char '\'') $
    MkEntry
      <$> uuidP
      <*> (optionMaybe $ try (char '/') >> labelP)
      <*> (optionMaybe $ try (char '#') >> labelP)
  where
    labelP = T.pack <$> (many1 $ noneOf "'/#\\")

stringP :: GenParser Char st Text
stringP =
  between (try $ char '"') (char '"') $
    T.pack <$> many (escape '"' <|> escape '\\' <|> noneOf "\"")
  where
    escape c = (try $ char '\\' >> char c) *> return c

numberP :: GenParser Char st Atom
numberP = do
  num :: Integer <- numP
  mdot :: Maybe String <- optionMaybe $ try (char '.') >> many digit
  expo :: Integer <- (try (char 'e') >> numP) <|> return 0
  case mdot of
    Just dot ->
      let e0 = toInteger $ length dot
       in let e = fromInteger $ expo - e0
           in let base = num * (10 ^ e0) + readE dot
               in return $ Number $ fromInteger base * (10.0 ** e)
    Nothing
      | expo >= 0 ->
          return $ Int $ num * (10 ^ expo)
    Nothing ->
      fail "Negative exponents only supported for floating point numbers"
  where
    readE str = if length str == 0 then 0 else read str
    numP = do
      sign <- optionMaybe $ try $ char '-'
      num <- many1 digit
      return $ (if isJust sign then -1 else 1) * read num

varP :: GenParser Char st Text
varP = do
  st <- upper
  tl <- many (alphaNum <|> oneOf "_-")
  return $ T.pack $ st : tl

atomP :: GenParser Char st Atom
atomP =
  Entry <$> entryP
    <|> Str <$> stringP
    <|> numberP
    <|> Var <$> varP

identP :: GenParser Char st Text
identP = T.pack <$> ((:) <$> baseP <*> many inP)
  where
    baseP = letter <|> oneOf symbols
    inP = alphaNum <|> oneOf symbols
    symbols = "_-"

predP :: GenParser Char st Predicate
predP = do
  name <- identP
  args <- optionMaybe $ do
    _ <- try $ spaces >> char '('
    _ <- spaces
    args <- sepEndBy atomP (try (spaces >> char ',') >> spaces)
    _ <- spaces
    _ <- char ')'
    return args
  return $ MkPred name $ case args of
    Just lst -> lst
    Nothing -> []

constrP :: GenParser Char st Constraint
constrP = negP <|> binopP
  where
    negP = Negated <$> (try (char '!') *> spaces *> predP)
    binopP = try $ do
      arg1 <- atomP
      _ <- spaces
      op <- string "=" *> return Equality <|> string "!=" *> return Difference
      _ <- spaces
      arg2 <- atomP
      return $ op arg1 arg2

data RulePart
  = RPPredicate Predicate
  | RPConstraint Constraint

ruleP :: GenParser Char st Rule
ruleP = do
  hd <- predP
  _ <- spaces
  args <- optionMaybe $ do
    _ <- try $ string ":-"
    _ <- spaces
    preds <- sepEndBy rulePartP (try (spaces >> char ',') >> spaces)
    _ <- spaces
    return $ foldl splitParts ([], []) preds
  _ <- char '.'
  return $ case args of
    Just (spreds, scstrs) -> MkRule hd (reverse spreds) (reverse scstrs)
    Nothing -> MkRule hd [] []
  where
    rulePartP = RPConstraint <$> constrP <|> RPPredicate <$> predP
    splitParts (preds, cstrs) (RPPredicate prd) = (prd : preds, cstrs)
    splitParts (preds, cstrs) (RPConstraint cstr) = (preds, cstr : cstrs)

programP :: GenParser Char st [Rule]
programP = spaces *> sepEndBy ruleP spaces <* eof

parseProgram :: String -> Either ParseError [Rule]
parseProgram = parse programP "(unknown)"
