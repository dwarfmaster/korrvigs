module Korrvigs.FTS.Query (Query (..), sqlQuery) where

import Data.Text (Text)
import Korrvigs.FTS.SQL
import Opaleye

data Query
  = Phrase [Text]
  | And [Query]
  | Or [Query]
  | Not Query
  deriving (Eq, Show)

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
