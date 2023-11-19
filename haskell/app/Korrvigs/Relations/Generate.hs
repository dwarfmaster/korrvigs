module Korrvigs.Relations.Generate (generateRelationsSql, generateRelationsHs) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (toUpper)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Database.PostgreSQL.Simple (Connection)
import Dhall hiding (map, sequence)
import Korrvigs.Relations.Parser

generateRelationsHs :: MonadIO m => Connection -> FilePath -> m Text
generateRelationsHs conn root = liftIO $ do
  res <- parseRelations conn root
  pure . toStrict . toLazyText $ case res of
    Right rels -> renderRelationsHs rels
    Left err -> "Error reading relations : " <> fromString err

generateRelationsSql :: MonadIO m => Connection -> FilePath -> m Text
generateRelationsSql conn root = liftIO $ do
  res <- parseRelations conn root
  pure . toStrict . toLazyText $ case res of
    Right rels -> renderRelationsSql rels
    Left err -> "Error reading relations : " <> fromString err

unlinesB :: [Builder] -> Builder
unlinesB = mconcat . intersperse "\n"

renderRelationsSql :: Map Text [(String, Type)] -> Builder
renderRelationsSql rels = unlinesB $ uncurry renderRelationSql <$> M.toList rels

renderRelationSql :: Text -> [(String, Type)] -> Builder
renderRelationSql rel lns =
  "create table if not exists "
    <> fromText rel
    <> " (\n"
    <> (mconcat . intersperse ",\n" $ uncurry renderRelationColSql <$> lns)
    <> "\n);\n"

renderRelationColSql :: String -> Type -> Builder
renderRelationColSql nm (TClass _) =
  "  " <> fromString nm <> " serial not null references entities on delete cascade"
renderRelationColSql nm TText = "  " <> fromString nm <> " text"
renderRelationColSql nm TUUID = "  " <> fromString nm <> " uuid"

renderRelationsHs :: Map Text [(String, Type)] -> Builder
renderRelationsHs rels =
  unlinesB $
    [ "module Korrvigs.Relations.Generated where",
      "",
      "import Opaleye",
      "import Data.Profunctor.Product",
      ""
    ]
      ++ (uncurry renderRelationHs <$> M.toList rels)

renderRelationHs :: Text -> [(String, Type)] -> Builder
renderRelationHs rel lns =
  hsName
    <> " ::\n  Table\n    ("
    <> fields
    <> ")\n    ("
    <> fields
    <> ")\n"
    <> hsName
    <> " =\n  table\n    \""
    <> fromText rel
    <> "\"\n    ( p"
    <> fromString (show $ length lns)
    <> "\n        ( "
    <> tableFields
    <> "\n        )\n    )\n"
  where
    hsName :: Builder
    hsName = (<> "Rel") $ fromString $ toSnakeCase $ T.unpack rel
    fields :: Builder
    fields = mconcat . intersperse ", " $ mkField . snd <$> lns
    mkField :: Type -> Builder
    mkField (TClass _) = "Field SqlInt8"
    mkField TText = "Field SqlText"
    mkField TUUID = "Field SqlUuid"
    tableFields :: Builder
    tableFields =
      mconcat . intersperse ",\n          " $
        ("tableField \"" <>) . (<> "\"") . fromString . fst <$> lns
    toSnakeCase :: String -> String
    toSnakeCase [] = []
    toSnakeCase ('_' : c : hs) = toUpper c : toSnakeCase hs
    toSnakeCase (c : hs) = c : toSnakeCase hs
