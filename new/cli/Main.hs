module Main where

import Control.Lens ((.~), (^.))
import Control.Monad (void)
import Data.Int (Int64)
import Data.Profunctor.Product.Default ()
import Data.Text (Text)
import Data.Text.IO (putStr)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Korrvigs.Entry
import Korrvigs.FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import Linear.V2
import Opaleye
import Prelude hiding (putStr)

toInsert :: Insert Int64
toInsert =
  Insert
    { iTable = entriesTable,
      iRows =
        toFields
          <$> [ mkEntryRow "H2" Note Nothing Nothing (Just (GeoPoint $ V2 4.20 3.1415)) Nothing Nothing
              ],
      iReturning = rCount,
      iOnConflict = Just doNothing
    }

setTextFor :: Text -> Text -> Update Int64
setTextFor nm txt =
  Update
    { uTable = entriesTable,
      uUpdateWith = sqlEntryText .~ toNullable (tsParseEnglish $ sqlStrictText txt),
      uWhere = \er -> (er ^. sqlEntryName) .== sqlStrictText nm,
      uReturning = rCount
    }

query :: Query
query = And [Phrase ["big", "rat"], Phrase ["bites"]]

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname='korrvigs_new'"
  res <- runSelect conn $ do
    EntryRow nm _ _ _ geog txt _ <- selectTable entriesTable
    where_ $ matchNullable (sqlBool True) (sqlQuery query @@) txt
    pure (nm, matchNullable (sqlDouble 0) (stAzimuth (sqlPoint $ V2 0 0)) geog)
  void $ runUpdate conn $ setTextFor "H2" "A big rat bited me in the rats"
  case parseQuery "\"big rat\" and bites or not surmulot" of
    Left err -> putStr $ err <> "\n"
    Right r -> putStrLn $ "Parsed: " <> show r
  case res :: [(Text, Double)] of
    [] -> putStrLn "No result"
    _ -> print res
  close conn
