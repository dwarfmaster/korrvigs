module Main where

import Data.Int (Int64)
import Data.Profunctor.Product.Default ()
import Data.Text (Text)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Korrvigs.Entry
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

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname='korrvigs_new'"
  res <- runSelect conn $ do
    EntryRow nm _ _ _ geog _ _ <- selectTable entriesTable
    pure (nm, geog)
  case res :: [(Text, Maybe Geometry)] of
    [] -> putStrLn "No result"
    _ -> print res
  close conn
