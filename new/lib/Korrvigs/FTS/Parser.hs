module Korrvigs.FTS.Parser (parseQuery) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.FTS.Query
import Text.Parsec

type Parser a = Parsec Text () a

parseQuery :: Text -> Either Text Query
parseQuery txt = case parse queryP "query" txt of
  Right q -> Right q
  Left err -> Left . T.pack $ show err

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
queryPhraseP = try (And <$> sepBy1 queryNotP spaces1) <|> queryNotP

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
