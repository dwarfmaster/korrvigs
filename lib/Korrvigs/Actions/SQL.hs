module Korrvigs.Actions.SQL (load, loadMetadata, removeKindDB, removeDB) where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import GHC.Int (Int64)
import qualified Korrvigs.Calendar.SQL as Cal
import Korrvigs.Entry
import qualified Korrvigs.Event.SQL as Event
import qualified Korrvigs.File.SQL as File
import Korrvigs.Kind
import qualified Korrvigs.Link.SQL as Link
import Korrvigs.Monad
import qualified Korrvigs.Note.SQL as Note
import Opaleye

mkEntry :: (IsKindData a) => EntryRow -> (Entry -> a) -> Entry
mkEntry row = kdEntry . entryFromRow kdKindData row

load :: (MonadKorrvigs m) => Id -> m (Maybe Entry)
load i = do
  mrow <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    pure entry
  case mrow of
    Nothing -> pure Nothing
    Just row -> do
      case row ^. sqlEntryKind of
        Note -> Note.sqlLoad i $ mkEntry row
        Link -> Link.sqlLoad i $ mkEntry row
        File -> File.sqlLoad i $ mkEntry row
        Event -> Event.sqlLoad i $ mkEntry row
        Calendar -> Cal.sqlLoad i $ mkEntry row

loadMetadata :: (MonadKorrvigs m) => Id -> m Metadata
loadMetadata i = do
  mtdt <- rSelect $ do
    mtdtRow <- selectTable entriesMetadataTable
    where_ $ mtdtRow ^. sqlEntry .== sqlId i
    pure (mtdtRow ^. sqlKey, mtdtRow ^. sqlValue)
  pure $ M.fromList mtdt

dispatchRemove :: (MonadKorrvigs m) => (Id -> [Delete Int64] -> m ()) -> KindData -> m ()
dispatchRemove rm (LinkD lnk) =
  let i = lnk ^. linkEntry . name in rm i $ Link.sqlRemove i
dispatchRemove rm (NoteD note) =
  let i = note ^. noteEntry . name in rm i $ Note.sqlRemove i
dispatchRemove rm (FileD file) =
  let i = file ^. fileEntry . name in rm i $ File.sqlRemove i
dispatchRemove rm (EventD ev) =
  let i = ev ^. eventEntry . name in rm i $ Event.sqlRemove i
dispatchRemove rm (CalendarD cal) =
  let i = cal ^. calEntry . name in rm i $ Cal.sqlRemove i

removeKindDB :: (MonadKorrvigs m) => Entry -> m ()
removeKindDB entry = dispatchRemove (\_ dels -> atomicSQL $ \conn -> forM_ dels $ runDelete conn) $ entry ^. kindData

genRemoveDB :: (MonadKorrvigs m) => Id -> [Delete Int64] -> m ()
genRemoveDB i dels =
  atomicSQL $ \conn -> do
    forM_ dels $ runDelete conn
    void $
      runDelete conn $
        Delete
          { dTable = entriesMetadataTable,
            dWhere = \mrow -> mrow ^. sqlEntry .== sqlId i,
            dReturning = rCount
          }
    void $
      runDelete conn $
        Delete
          { dTable = entriesTable,
            dWhere = \erow -> erow ^. sqlEntryName .== sqlId i,
            dReturning = rCount
          }

removeDB :: (MonadKorrvigs m) => Entry -> m ()
removeDB entry = dispatchRemove genRemoveDB $ entry ^. kindData
