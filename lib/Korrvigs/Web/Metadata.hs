module Korrvigs.Web.Metadata (getEntryMtdtR, postEntryMtdtR) where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Opaleye hiding (null)
import Yesod hiding (Update)

mvalueToJson :: MetadataValue -> Value
mvalueToJson mv = object ["value" .= (mv ^. metaValue), "readOnly" .= (mv ^. metaReadOnly)]

mtdtToJson :: Metadata -> Value
mtdtToJson mtdt = toJSON $ mvalueToJson <$> mtdt

getEntryMtdtR :: WebId -> Handler Value
getEntryMtdtR (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just entry ->
      lookupGetParam "key" >>= \case
        Nothing -> pure $ mtdtToJson $ entry ^. metadata
        Just key -> case M.lookup key (entry ^. metadata) of
          Nothing -> invalidArgs [key]
          Just val -> pure $ mvalueToJson val

data MtdtPost = MtdtPost
  { _mtdtAdd :: Map Text Value,
    _mtdtRm :: [Text]
  }
  deriving (Eq, Ord, Show)

makeLenses ''MtdtPost

instance FromJSON MtdtPost where
  parseJSON (Object v) =
    MtdtPost
      <$> v
        .: "insert"
      <*> v
        .: "remove"
  parseJSON invalid =
    prependFailure "parsing mtdt post value failed, " $ typeMismatch "Object" invalid

-- TODO metadata changes should be reflected on the files, not just in the database,
-- otherwise any change will be lost after the next sync
postEntryMtdtR :: WebId -> Handler Value
postEntryMtdtR (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just _ -> do
      mtdts <- requireCheckJsonBody :: Handler MtdtPost
      let mtdtKeys = (fst <$> M.toList (mtdts ^. mtdtAdd)) ++ mtdts ^. mtdtRm
      ros <- rSelect $ do
        mtdt <- selectTable entriesMetadataTable
        where_ $ mtdt ^. sqlEntry .== sqlId i
        where_ $ sqlElem (mtdt ^. sqlKey) $ sqlArray sqlStrictText mtdtKeys
        where_ $ mtdt ^. sqlReadOnly .== sqlBool True
        pure $ mtdt ^. sqlKey
      unless (null ros) $ invalidArgs ros
      let rows = mkRow <$> M.toList (mtdts ^. mtdtAdd)
      atomicSQL $ \conn -> do
        void $
          runDelete conn $
            Delete
              { dTable = entriesMetadataTable,
                dWhere = \mtdt -> sqlElem (mtdt ^. sqlKey) $ sqlArray sqlStrictText $ mtdts ^. mtdtRm,
                dReturning = rCount
              }
        void $
          runInsert conn $
            Insert
              { iTable = entriesMetadataTable,
                iRows = rows,
                iReturning = rCount,
                iOnConflict = Just doNothing
              }
      redirect $ EntryMtdtR $ WId i
  where
    mkRow :: (Text, Value) -> MetadataRowSQL
    mkRow (key, val) = MetadataRow (sqlId i) (sqlStrictText key) (sqlValueJSONB val) (sqlBool False)
