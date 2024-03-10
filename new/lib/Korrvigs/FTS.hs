module Korrvigs.FTS
  ( SqlTSVector,
    tsParseEnglish,
    SqlTSQuery,
    Query (..),
    sqlQuery,
    (@@),
    parseQuery,
    Korrvigs.FTS.tests,
  )
where

import Korrvigs.FTS.Parser
import Korrvigs.FTS.Query
import Korrvigs.FTS.SQL
import Test.HUnit

tests :: Test
tests =
  test
    ["Query" ~: Korrvigs.FTS.Query.tests]
