module Main where

import Data.Aeson (Value)
import Data.Int (Int64)
import Data.Profunctor.Product.Default ()
import Data.Text (Text)
import Data.Time (ZonedTime)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Korrvigs.Geometry
import Korrvigs.Kind.Entry
import Korrvigs.Kind.Schema
import Linear.V2
import Opaleye

toInsert :: Insert Int64
toInsert =
  Insert
    { iTable = entriesTable,
      iRows =
        toFields
          <$> [ ("H2" :: Text, Note, Nothing :: Maybe ZonedTime, Just (GeoPoint $ V2 4.20 3.1415), Nothing :: Maybe Text, Nothing :: Maybe Value)
              ],
      iReturning = rCount,
      iOnConflict = Just doNothing
    }

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname='korrvigs_new'"
  res <- runSelect conn $ do
    (nm, _, _, geog, _, _) <- selectTable entriesTable
    pure (nm, geog)
  case res :: [(Text, Maybe Geometry)] of
    [] -> putStrLn "No result"
    _ -> print res
  close conn
