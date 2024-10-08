module Korrvigs.FTS
  ( SqlTSVector,
    tsParseEnglish,
    tsRank,
    SqlTSQuery,
    Query (..),
    sqlQuery,
    (@@),
    (@@?),
    parseQuery,
    renderQuery,
    Korrvigs.FTS.tests,
  )
where

import Korrvigs.FTS.Parser
import Korrvigs.FTS.Query
import Korrvigs.FTS.Render
import Korrvigs.FTS.SQL
import Test.HUnit

tests :: Test
tests =
  test
    [ "Query" ~: Korrvigs.FTS.Query.tests,
      "Parser" ~: Korrvigs.FTS.Parser.tests
    ]
