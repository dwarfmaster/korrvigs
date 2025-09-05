module Korrvigs.Metadata.Task where

import Control.Lens
import Data.Aeson
import Data.CaseInsensitive (CI)
import Data.Maybe
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.TH
import Korrvigs.Monad
import Korrvigs.Utils.JSON (fromJSONM)
import Opaleye (sqlInt4)

data TaskStatus
  = TaskTodo
  | TaskImportant
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
parseStatusName "important" = Just TaskImportant
parseStatusName "started" = Just TaskOngoing
parseStatusName "blocked" = Just TaskBlocked
parseStatusName "done" = Just TaskDone
parseStatusName "dont" = Just TaskDont
parseStatusName _ = Nothing

mkMtdt "TaskMtdt" "task" [t|Text|]
mkMtdt "TaskDeadline" "deadline" [t|ZonedTime|]
mkMtdt "TaskScheduled" "scheduled" [t|ZonedTime|]
mkMtdt "TaskStarted" "started" [t|ZonedTime|]
mkMtdt "TaskFinished" "finished" [t|ZonedTime|]

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
loadTask :: (MonadKorrvigs m) => Id -> Int -> Maybe Text -> m (Maybe Task)
loadTask i sqlI title = do
  let si = sqlInt4 sqlI
  r <-
    rSelectOne $
      (,,,,)
        <$> baseSelectTextMtdt TaskMtdt si
        <*> selectMtdt TaskDeadline si
        <*> selectMtdt TaskScheduled si
        <*> selectMtdt TaskStarted si
        <*> selectMtdt TaskFinished si
  case r of
    Nothing -> pure Nothing
    Just (tsk, deadline, scheduled, started, finished) -> do
      let st = fromMaybe TaskTodo $ parseStatusName tsk
      let title' = fromMaybe ("@" <> unId i) title
      let deadline' = deadline >>= fromJSONM
      let scheduled' = scheduled >>= fromJSONM
      let started' = started >>= fromJSONM
      let finished' = finished >>= fromJSONM
      pure $ Just $ Task st tsk title' deadline' scheduled' started' finished'
