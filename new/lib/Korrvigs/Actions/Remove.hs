module Korrvigs.Actions.Remove where

import Control.Lens
import Korrvigs.AllEntries ()
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Monad hiding (dispatchRemove, dispatchRemoveDB, remove, removeDB)

dispatchRemove :: (MonadKorrvigs m) => Entry -> m ()
dispatchRemove entry =
  case entry ^. kindData of
    NoteD note -> dRemove (dIdentify note)
    LinkD link -> dRemove (dIdentify link)

remove :: (MonadKorrvigs m) => Id -> m ()
remove i = load i >>= mapM_ dispatchRemove

dispatchRemoveDB :: (MonadKorrvigs m) => Entry -> m ()
dispatchRemoveDB entry =
  case entry ^. kind of
    Note -> dRemoveDB (Nothing :: Maybe Note) i
    Link -> dRemoveDB (Nothing :: Maybe Link) i
  where
    i = entry ^. name

removeDB :: (MonadKorrvigs m) => Id -> m ()
removeDB i = load i >>= mapM_ dispatchRemoveDB
