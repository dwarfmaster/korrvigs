{-# LANGUAGE DeriveGeneric #-}

module Korrvigs.Relations.Parser (Type (..), parseDhall, parseRelation, parseRelations) where

import Control.Exception (TypeError (..), catch)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import Dhall hiding (map, sequence)
import Korrvigs.Classes
import qualified Korrvigs.DB as DB
import Korrvigs.Definition
import Korrvigs.Schema
import Opaleye ((.==))
import qualified Opaleye as O
import System.Directory (makeAbsolute)

data DhallColumn = DC
  { col :: String,
    tp :: String
  }
  deriving (Generic, Show)

instance FromDhall DhallColumn

readDhall :: FilePath -> IO (Either String [DhallColumn])
readDhall path =
  liftIO $ do
    absolute <- makeAbsolute path
    catch (Right <$> input auto (T.pack absolute)) $
      \(TypeError e) -> pure $ Left e

data Type
  = TClass Class
  | TText
  | TUUID
  deriving (Show)

parseType :: String -> Either String Type
parseType (':' : cls) =
  case parse $ T.pack cls of
    Just c -> Right $ TClass c
    Nothing -> Left $ "Unknown class " <> cls
parseType "Text" = Right TText
parseType "UUID" = Right TUUID
parseType typ = Left $ "Type " <> typ <> " not supported"

parseColumn :: DhallColumn -> Either String (String, Type)
parseColumn (DC cl typ) = (cl,) <$> parseType typ

parseDhall :: FilePath -> IO (Either String [(String, Type)])
parseDhall path = readDhall path >>= \dhall -> pure $ mapM parseColumn =<< dhall

sqlRelations :: O.Select (O.Field O.SqlText, O.Field O.SqlUuid)
sqlRelations = do
  (uuid_, name_, _) <- O.selectTable entriesTable
  (_, _, entry_, sub_, query_) <- O.selectTable entitiesTable
  O.where_ $ uuid_ .== entry_
  O.where_ $ O.isNull query_
  O.where_ $
    O.matchNullable
      (O.sqlBool False)
      (.== O.sqlStrictText "relation.dhall")
      sub_
  (_, cls_) <- DB.rootFor entry_
  O.where_ $ cls_ .== O.sqlStrictText (name OntologyRelation)
  pure (name_, entry_)

parseRelation ::
  Connection ->
  FilePath ->
  Text ->
  IO (Either String [(String, Type)])
parseRelation conn root relName = do
  res <- O.runSelect conn $ do
    (name_, entry_) <- sqlRelations
    O.where_ $ name_ .== O.sqlStrictText relName
    pure entry_
  case res of
    [entry] -> parseDhall $ subPath root entry "relation.dhall"
    _ -> pure $ Left $ "Couldn't find relation " <> T.unpack relName

parseRelations ::
  Connection ->
  FilePath ->
  IO (Either String (Map Text [(String, Type)]))
parseRelations conn root = do
  res <- O.runSelect conn sqlRelations
  parsed <- mapM prepare res
  pure $ M.fromList <$> sequence parsed
  where
    prepare :: (Text, UUID) -> IO (Either String (Text, [(String, Type)]))
    prepare (nm, entry) = do
      res <- parseDhall $ subPath root entry "relation.dhall"
      pure $ (nm,) <$> res
