module Korrvigs.Web.Metadata (getEntryMtdtR, postEntryMtdtR) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Actions.Metadata
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
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

postEntryMtdtR :: WebId -> Handler Value
postEntryMtdtR (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> do
      mtdts <- requireCheckJsonBody :: Handler MtdtPost
      r <- updateMetadata entry (mtdts ^. mtdtAdd) (mtdts ^. mtdtRm)
      case r of
        Left ros -> invalidArgs ros
        Right _ -> redirect $ EntryMtdtR $ WId i
