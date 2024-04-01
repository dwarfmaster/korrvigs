module Korrvigs.Link.Sync where

import qualified Data.Text as T
import Korrvigs.Entry (Id (MkId))
import Korrvigs.Monad (MonadKorrvigs)
import System.FilePath (takeBaseName)

linkIdFromPath :: FilePath -> Id
linkIdFromPath = MkId . T.pack . takeBaseName

syncLink :: MonadKorrvigs m => FilePath -> m ()
syncLink path = do
  let i = linkIdFromPath path
  pure undefined
