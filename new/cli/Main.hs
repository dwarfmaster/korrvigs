module Main where

import Control.Lens ((.~), (^.))
import Control.Monad (void)
import Data.Int (Int64)
import Data.Profunctor.Product.Default ()
import Data.Text (Text)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Korrvigs.Entry
import Korrvigs.FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import Linear.V2
import Opaleye

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

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname='korrvigs_new'"
  res <- runSelect conn $ do
    EntryRow nm _ _ _ geog _ _ <- selectTable entriesTable
    pure (nm, matchNullable (sqlDouble 0) (stAzimuth (sqlPoint $ V2 0 0)) geog)
  void $ runUpdate conn $ setTextFor "H2" "A big rat bited me in the rats"
  case res :: [(Text, Double)] of
    [] -> putStrLn "No result"
    _ -> print res
  close conn
