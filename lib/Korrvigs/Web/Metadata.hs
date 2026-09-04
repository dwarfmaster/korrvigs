module Korrvigs.Web.Metadata (getEntryMtdtR, postEntryMtdtR) where

import Control.Arrow (first)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens hiding (key)
import Data.Aeson.Types
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format.ISO8601
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Monad.Metadata
import Korrvigs.Utils.Time
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

handlePostTask :: Id -> MtdtPost -> Handler MtdtPost
handlePostTask i mtdts | isJust (mtdts ^? mtdtAdd . at (mtdtSqlName TaskMtdt) . _Just) = do
  hasStarted <- fmap isJust $ rSelectMtdt TaskStarted $ sqlId i
  oldTask <- fmap (fromMaybe TaskTodo) $ rSelectMtdt TaskMtdt $ sqlId i
  let newTask = fromMaybe TaskTodo $ mtdts ^? mtdtAdd . at (mtdtSqlName TaskMtdt) . _Just . _JSON
  let setStarted = isTaskTodo oldTask && not (isTaskTodo newTask) && not hasStarted
  let setFinished = isTaskDone newTask && not (isTaskDone oldTask)
  time <- liftIO getCurrentZonedTime
  let renderedTime = T.pack $ iso8601Show time
  let setIf b f = if b then f else id
  pure $
    mtdts
      & setIf setStarted (mtdtAdd . at (mtdtSqlName TaskStarted) ?~ toJSON renderedTime)
      & setIf setFinished (mtdtAdd . at (mtdtSqlName TaskFinished) ?~ toJSON renderedTime)
handlePostTask _ mtdts = pure mtdts

postEntryMtdtR :: WebId -> Handler Value
postEntryMtdtR (WId i) =
  load i >>= \case
    Nothing -> notFound
    Just entry -> do
      mtdts <- requireCheckJsonBody :: Handler MtdtPost
      actual <- handlePostTask i mtdts
      updateMetadata entry (actual ^. mtdtAdd) (actual ^. mtdtRm)
      redirect $ EntryMtdtR $ WId i
