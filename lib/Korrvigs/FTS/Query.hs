module Korrvigs.FTS.Query (Query (..), sqlQuery, tests) where

import Data.Aeson
import Data.Text (Text)
import Korrvigs.FTS.SQL
import Opaleye
import Test.HUnit

data Query
  = Phrase [Text]
  | And [Query]
  | Or [Query]
  | Not Query
  deriving (Eq, Show)

instance ToJSON Query where
  toJSON (Phrase txts) = object ["kind" .= ("phrase" :: Text), "value" .= txts]
  toJSON (And qs) = object ["kind" .= ("and" :: Text), "value" .= qs]
  toJSON (Or qs) = object ["kind" .= ("or" :: Text), "value" .= qs]
  toJSON (Not q) = object ["kind" .= ("not" :: Text), "value" .= q]

instance FromJSON Query where
  parseJSON = withObject "FTS Query" $ \obj ->
    (obj .: "kind") >>= \case
      "phrase" -> Phrase <$> obj .: "value"
      "and" -> And <$> obj .: "value"
      "or" -> Or <$> obj .: "value"
      "not" -> Not <$> obj .: "value"
      s -> fail $ s <> " is not a valid FTS query"

sqlQuery :: Query -> Field SqlTSQuery
sqlQuery = pgQuery . compile

compile :: Query -> TSQuery
compile (Phrase []) = TSText ""
compile (Phrase txts) = foldl1 TSSeq $ TSText <$> txts
compile (And []) = TSText ""
compile (And queries) = foldl1 TSAnd $ compile <$> queries
compile (Or []) = TSText ""
compile (Or queries) = foldl1 TSOr $ compile <$> queries
compile (Not q) = TSNot $ compile q

tests :: Test
tests =
  TestCase $
    compile (And [Phrase ["a"], Not (Phrase ["b"]), Phrase ["c", "d"]])
      @=? TSAnd
        (TSAnd (TSText "a") (TSNot (TSText "b")))
        (TSSeq (TSText "c") (TSText "d"))
