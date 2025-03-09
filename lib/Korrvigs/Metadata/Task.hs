module Korrvigs.Metadata.Task where

import Control.Lens
import Data.Aeson
import Data.CaseInsensitive (CI)
import Data.Maybe
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Utils.JSON (fromJSONM)

data TaskStatus
  = TaskTodo
  | TaskOngoing
  | TaskBlocked
  | TaskDone
  | TaskDont
  deriving (Eq, Show, Bounded, Enum, Ord)

makePrisms ''TaskStatus

data Task = Task
  { _tskStatus :: TaskStatus,
    _tskStatusName :: Text,
    _tskLabel :: Text,
    _tskDeadline :: Maybe ZonedTime,
    _tskScheduled :: Maybe ZonedTime,
    _tskStarted :: Maybe ZonedTime,
    _tskFinished :: Maybe ZonedTime
  }
  deriving (Show)

makeLenses ''Task

parseStatusName :: Text -> Maybe TaskStatus
parseStatusName "todo" = Just TaskTodo
parseStatusName "started" = Just TaskOngoing
parseStatusName "blocked" = Just TaskBlocked
parseStatusName "done" = Just TaskDone
parseStatusName "dont" = Just TaskDont
parseStatusName _ = Nothing

data TaskMtdt = TaskMtdt

instance ExtraMetadata TaskMtdt where
  type MtdtType TaskMtdt = Text
  mtdtName = const "task"

data TaskDeadline = TaskDeadline

instance ExtraMetadata TaskDeadline where
  type MtdtType TaskDeadline = ZonedTime
  mtdtName = const "deadline"

data TaskScheduled = TaskScheduled

instance ExtraMetadata TaskScheduled where
  type MtdtType TaskScheduled = ZonedTime
  mtdtName = const "scheduled"

data TaskStarted = TaskStarted

instance ExtraMetadata TaskStarted where
  type MtdtType TaskStarted = ZonedTime
  mtdtName = const "started"

data TaskFinished = TaskFinished

instance ExtraMetadata TaskFinished where
  type MtdtType TaskFinished = ZonedTime
  mtdtName = const "finished"

applyTaskMtdt :: (CI Text -> Maybe Value) -> Task -> Task
applyTaskMtdt f =
  extractAttr TaskDeadline tskDeadline
    . extractAttr TaskScheduled tskScheduled
    . extractAttr TaskStarted tskStarted
    . extractAttr TaskFinished tskFinished
  where
    extractAttr :: (ExtraMetadata a, FromJSON (MtdtType a)) => a -> ASetter Task Task (Maybe (MtdtType a)) (Maybe (MtdtType a)) -> Task -> Task
    extractAttr attr setter =
      maybe id (setter ?~) (f (mtdtName attr) >>= fromJSONM)

-- Load task from SQL
loadTask :: (MonadKorrvigs m) => Id -> m (Maybe Task)
loadTask i = do
  let si = sqlId i
  r <-
    rSelectOne $
      (,,,,,)
        <$> baseSelectTextMtdt TaskMtdt si
        <*> selectTextMtdt Title si
        <*> selectMtdt TaskDeadline si
        <*> selectMtdt TaskScheduled si
        <*> selectMtdt TaskStarted si
        <*> selectMtdt TaskFinished si
  case r of
    Nothing -> pure Nothing
    Just (Nothing, _, _, _, _, _) -> pure Nothing
    Just (Just tsk, title, deadline, scheduled, started, finished) -> do
      let st = fromMaybe TaskTodo $ parseStatusName tsk
      let title' = fromMaybe ("@" <> unId i) title
      let deadline' = deadline >>= fromJSONM
      let scheduled' = scheduled >>= fromJSONM
      let started' = started >>= fromJSONM
      let finished' = finished >>= fromJSONM
      pure $ Just $ Task st tsk title' deadline' scheduled' started' finished'
