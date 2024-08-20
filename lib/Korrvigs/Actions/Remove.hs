module Korrvigs.Actions.Remove where

import Control.Lens
import Control.Monad (forM_, void)
import GHC.Int (Int64)
import Korrvigs.AllEntries ()
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Monad hiding (dispatchRemove, dispatchRemoveDB, remove, removeDB)
import Opaleye

dispatchRemove :: (MonadKorrvigs m) => Entry -> m ()
dispatchRemove entry =
  case entry ^. kindData of
    NoteD note -> dRemove (dIdentify note)
    LinkD link -> dRemove (dIdentify link)
    FileD file -> dRemove (dIdentify file)

remove :: (MonadKorrvigs m) => Id -> m ()
remove i = load i >>= mapM_ dispatchRemove

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

dispatchRemoveDB :: (MonadKorrvigs m) => Entry -> m ()
dispatchRemoveDB entry =
  case entry ^. kind of
    Note -> genRemoveDB i $ dRemoveDB (Nothing :: Maybe Note) i
    Link -> genRemoveDB i $ dRemoveDB (Nothing :: Maybe Link) i
    File -> genRemoveDB i $ dRemoveDB (Nothing :: Maybe File) i
  where
    i = entry ^. name

removeDB :: (MonadKorrvigs m) => Id -> m ()
removeDB i = load i >>= mapM_ dispatchRemoveDB