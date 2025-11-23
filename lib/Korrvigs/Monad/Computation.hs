module Korrvigs.Monad.Computation where

import Conduit
import Control.Lens
import Data.Text (Text)
import Korrvigs.Compute.Computation
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.Type
import Korrvigs.Entry
import qualified Korrvigs.File.Sync as File
import Korrvigs.Monad.Class
import Korrvigs.Monad.SQL
import qualified Korrvigs.Note.Sync as Note

getComputation :: (MonadKorrvigs m) => Id -> Text -> m (Maybe Computation)
getComputation i cmp =
  load i >>= \case
    Nothing -> pure Nothing
    Just entry -> case entry ^. entryKindData of
      NoteD note -> Note.getComputation note cmp
      FileD file -> File.getComputation file cmp
      _ -> pure Nothing

storeComputationResult :: (MonadKorrvigs m) => Id -> Text -> RunnableType -> Hash -> RunnableResult -> m ()
storeComputationResult i cmp tp hsh res = do
  entry <- load i >>= throwMaybe (KCantLoad i "Failed to load entry")
  case entry ^. entryKindData of
    NoteD note -> Note.storeComputationResult note cmp tp hsh res
    FileD file -> File.storeComputationResult file cmp tp hsh res
    _ -> throwM $ KMiscError $ "Tried to save computation to " <> unId i <> ", which is neither a note nor a file"
