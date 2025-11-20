module Korrvigs.Monad.Computation where

import Data.Text (Text)
import Korrvigs.Compute.Computation
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.Type
import Korrvigs.Entry
import Korrvigs.Monad.Class

getComputation :: (MonadKorrvigs m) => Id -> Text -> m (Maybe Computation)
getComputation = undefined

storeComputationResult :: (MonadKorrvigs m) => Id -> Text -> Hash -> RunnableResult -> m ()
storeComputationResult = undefined
