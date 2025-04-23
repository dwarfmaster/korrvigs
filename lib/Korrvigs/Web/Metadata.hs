module Korrvigs.Web.Metadata (getEntryMtdtR, postEntryMtdtR) where

import Control.Arrow (first)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Actions
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod hiding (Update)

getEntryMtdtR :: WebId -> Handler Value
getEntryMtdtR (WId i) = do
  mtdt <- loadMetadata i
  lookupGetParam "key" >>= \case
    Nothing -> pure $ toJSON $ M.fromList $ first CI.foldedCase <$> M.toList mtdt
    Just key -> case M.lookup (CI.mk key) mtdt of
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
