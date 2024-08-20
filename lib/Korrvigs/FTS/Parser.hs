module Korrvigs.FTS.Parser (parseQuery, tests) where

import Control.Monad (void)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.FTS.Query hiding (tests)
import Test.HUnit
import Text.Parsec

type Parser a = Parsec Text () a

parseQuery :: Text -> Either Text Query
parseQuery txt = case parse queryP "query" txt of
  Right q -> Right q
  Left err -> Left . T.pack $ show err

tests :: Test
tests =
  test
    [ "Fail" ~: isLeft (parseQuery "or") @? "Expect failure",
      "Basic"
        ~: parseQuery "toto tata hello"
        @=? Right (And [Phrase ["toto"], Phrase ["tata"], Phrase ["hello"]]),
      "Logic"
        ~: parseQuery "toto and tata or not titi and tutu"
        @=? Right
          ( Or
              [ And [Phrase ["toto"], Phrase ["tata"]],
                And [Not (Phrase ["titi"]), Phrase ["tutu"]]
              ]
          ),
      "Quote"
        ~: parseQuery "\"a b c d\""
        @=? Right (Phrase ["a", "b", "c", "d"])
    ]

queryP :: Parser Query
queryP = queryOrP <* eof

spaces1 :: Parser ()
spaces1 = void $ many1 space

queryOrP :: Parser Query
queryOrP = try orP <|> queryAndP
  where
    orP :: Parser Query
    orP = do
      q1 <- queryAndP
      spaces1
      void $ string "or"
      spaces1
      q2 <- queryOrP
      pure . Or $ case q2 of
        Or qs -> q1 : qs
        _ -> [q1, q2]

queryAndP :: Parser Query
queryAndP = try andP <|> queryPhraseP
  where
    andP :: Parser Query
    andP = do
      q1 <- queryPhraseP
      spaces1
      void $ string "and"
      spaces1
      q2 <- queryAndP
      pure . And $ case q2 of
        And qs -> q1 : qs
        _ -> [q1, q2]

queryPhraseP :: Parser Query
queryPhraseP = try (andC <$> sepBy1 queryNotP spaces1) <|> queryNotP
  where
    andC :: [Query] -> Query
    andC [q] = q
    andC qs = And qs

queryNotP :: Parser Query
queryNotP = try notP <|> queryQuoteP
  where
    notP :: Parser Query
    notP = do
      void $ string "not"
      spaces1
      q <- queryNotP
      pure $ case q of
        Not nq -> nq
        _ -> Not q

wordP :: Parser Text
wordP = T.pack <$> many1 alphaNum

queryQuoteP :: Parser Query
queryQuoteP = try quoteP <|> queryWordP
  where
    quoteP :: Parser Query
    quoteP =
      Phrase
        <$> between
          (char '"')
          (char '"')
          (sepBy1 wordP spaces1)

queryWordP :: Parser Query
queryWordP = try (fmap (Phrase . (: [])) $ wordP >>= selP) <|> queryBottomP
  where
    selP :: Text -> Parser Text
    selP "and" = fail "And is a reserved keyword"
    selP "or" = fail "Or is a reserved keyword"
    selP "not" = fail "Not is a reserved keyword"
    selP t = pure t

queryBottomP :: Parser Query
queryBottomP = between (char '(' >> spaces1) (spaces1 >> char ')') queryP
