module Korrvigs.Cli.Query where

import Control.Arrow ((&&&))
import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Aeson.Encoding as JEnc
import qualified Data.ByteString.Lazy.UTF8 as BSL8
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Korrvigs.Cli.Info as Info
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New (parseCollection)
import Korrvigs.Entry
import qualified Korrvigs.FTS as FTS
import qualified Korrvigs.Format as Fmt
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Query
import Korrvigs.Utils.DateParser
import Options.Applicative
import System.Exit
import Text.Parsec (char, parse)
import Text.Parsec.Number

data Cmd = Cmd
  { _query :: Query,
    _format :: Maybe Text,
    _json :: Bool
  }

makeLenses ''Cmd

mapL :: (a -> c) -> Either a b -> Either c b
mapL f (Left x) = Left $ f x
mapL _ (Right x) = Right x

ftsQueryParser :: ReadM FTS.Query
ftsQueryParser = eitherReader $ mapL T.unpack . FTS.parseQuery . T.pack

mtdtQueryParser :: ReadM (Text, JsonQuery)
mtdtQueryParser = eitherReader $ mapL T.unpack . parseMtdtQuery . T.pack

kindNames :: Kind -> String
kindNames Note = "note"
kindNames File = "file"
kindNames Event = "event"
kindNames Calendar = "calendar"
kindNames Syndicate = "syndicate"

kindParser :: ReadM Kind
kindParser = eitherReader $ \s -> case M.lookup s names of
  Just kd -> pure kd
  Nothing -> Left $ "Kind must be one of " <> intercalate ", " (kindNames <$> [minBound .. maxBound])
  where
    names =
      M.fromList $ (kindNames &&& id) <$> [minBound .. maxBound]

rectangleParser :: ReadM Polygon
rectangleParser = eitherReader $ mapL show . parse rectangleP "<rectangle>"

withinParser :: ReadM (Point, Double)
withinParser =
  eitherReader $ mapL show . parse ((,) <$> pointP <*> (char ':' *> floating2 True)) "<within>"

criterionParser :: ReadM SortCriterion
criterionParser = eitherReader $ \case
  "date" -> Right ByDate
  "id" -> Right ById
  _ -> Left "Sort must be either by date or by id"

colParser :: ReadM QueryInCollection
colParser =
  eitherReader $ \s -> case parseCollection $ T.pack s of
    Nothing -> Left "Could not parse id#col"
    Just (i, c) -> Right $ QueryInCol c i

sortParser :: Parser (SortCriterion, SortOrder)
sortParser =
  (,)
    <$> option criterionParser (long "sort" <> help "Criterion to sort results by, default to id" <> value ById)
    <*> flag SortAsc SortDesc (long "descending" <> help "Invert sorting criterion")

queryParser :: Parser Query
queryParser =
  Query
    <$> many (MkId <$> option str (metavar "ID" <> long "id" <> help "ID the entry must have"))
    <*> optional (argument str (metavar "TITLE" <> help "Regex for title"))
    <*> optional (argument ftsQueryParser (metavar "FTS" <> help "Full text search"))
    <*> optional (option dayParser (long "before" <> help "Entry must have date before the provided date"))
    <*> optional (option dayParser (long "after" <> help "Entry must have date after the provided date"))
    <*> optional (option rectangleParser (long "inrect" <> help "Entries must be located in the given rectangle"))
    <*> optional (option withinParser (long "within" <> help "Filter entries within a certain distance in meters from a point"))
    <*> optional (option kindParser (long "kind" <> help "Entry must be of provided kind"))
    <*> many (option mtdtQueryParser (long "mtdt" <> help "Add metadata conditions"))
    <*> optional (option colParser (long "incol" <> help "Entries must be in collection"))
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> switch (long "show-hidden" <> help "Show hidden entries in search result")
    <*> sortParser
    <*> optional (option auto (long "limit" <> help "Limit the number of results"))

parser' :: Parser Cmd
parser' =
  Cmd
    <$> queryParser
    <*> optional (strOption (long "format" <> help "How to format found entries"))
    <*> switch (long "json" <> help "Each entry is displayed as JSON")

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Queries entries by some criterion"
      <> header "korr query -- Query entries"

run :: Cmd -> KorrM ()
run cmd = do
  fmt <- case Fmt.parse Fmt.entrySpec (fromMaybe defaultFormat $ cmd ^. format) of
    Right fmt -> pure fmt
    Left err -> liftIO $ do
      putStrLn $ "Failed to parse format: " <> T.unpack err
      exitFailure
  ids <- rSelect $ do
    entry <- compile (cmd ^. query) (pure . view sqlEntryName)
    pure $ entry ^. _2
  forM_ ids $ \i ->
    load i >>= \case
      Nothing -> liftIO $ TIO.putStrLn $ "Failed to load " <> unId i
      Just entry ->
        if not (cmd ^. json)
          then
            Fmt.run fmt entry >>= \case
              Just render -> liftIO $ TIO.putStrLn render
              Nothing -> pure ()
          else do
            mtdt <- loadMetadata i
            let mp = mconcat $ fmap (Info.buildInfoJSON entry mtdt) Info.entryInfoSpec
            let obj = JEnc.encodingToLazyByteString $ JEnc.value $ toJSON mp
            liftIO $ putStrLn $ BSL8.toString obj
  where
    defaultFormat = "[{kind}] {name}: {title::}"
