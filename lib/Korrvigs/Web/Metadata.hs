module Korrvigs.Web.Metadata (getEntryMtdtR, postEntryMtdtR) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Actions.Load (loadMetadata)
import Korrvigs.Actions.Metadata
import Korrvigs.Monad
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod hiding (Update)

getEntryMtdtR :: WebId -> Handler Value
getEntryMtdtR (WId i) = do
  mtdt <- loadMetadata i
  lookupGetParam "key" >>= \case
    Nothing -> pure $ toJSON mtdt
    Just key -> case M.lookup key mtdt of
      Nothing -> invalidArgs [key]
      Just val -> pure $ toJSON val

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
      updateMetadata entry (mtdts ^. mtdtAdd) (mtdts ^. mtdtRm)
      redirect $ EntryMtdtR $ WId i
