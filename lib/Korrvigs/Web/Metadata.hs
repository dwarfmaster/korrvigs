module Korrvigs.Web.Metadata (getEntryMtdtR, postEntryMtdtR, getEntryParentsR, postEntryParentsR) where

import Control.Arrow (first)
import Control.Lens hiding ((.=))
import Control.Monad (unless)
import Control.Monad.Loops (allM)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Actions
import Korrvigs.Entry
import Korrvigs.Monad
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

getEntryParentsR :: WebId -> Handler Value
getEntryParentsR (WId i) = do
  parents :: [Text] <- rSelect $ selectTargetsFor entriesSubTable i
  pure $ toJSON parents

data ParentPost = ParentPost
  { _parentAdd :: [Text],
    _parentRm :: [Text]
  }
  deriving (Eq, Ord, Show)

makeLenses ''ParentPost

instance FromJSON ParentPost where
  parseJSON (Object v) =
    ParentPost
      <$> v .: "add"
      <*> v .: "remove"
  parseJSON invalid =
    prependFailure "parsing parent post value failed, " $ typeMismatch "Object" invalid

postEntryParentsR :: WebId -> Handler ()
postEntryParentsR (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> do
      parents <- requireCheckJsonBody :: Handler ParentPost
      let toAdd = MkId <$> parents ^. parentAdd
      let toRm = MkId <$> parents ^. parentRm
      allExists <- allM (fmap (not . null) . load) $ toAdd ++ toRm
      unless allExists notFound
      updateParents entry toAdd toRm
      redirect $ EntryParentsR $ WId i
