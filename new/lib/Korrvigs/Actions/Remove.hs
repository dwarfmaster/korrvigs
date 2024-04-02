module Korrvigs.Actions.Remove where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Link ()
import Korrvigs.Monad

remove :: MonadKorrvigs m => Id -> m ()
remove i =
  load i >>= \case
    Nothing -> pure ()
    Just entry -> do
      case entry ^. kindData of
        NoteD _ -> undefined
        LinkD link -> dRemove (dIdentify link)

removeDB :: MonadKorrvigs m => Id -> m ()
removeDB i =
  load i >>= \case
    Nothing -> pure ()
    Just entry -> do
      case entry ^. kind of
        Note -> undefined
        Link -> dRemoveDB (Nothing :: Maybe Link) i
