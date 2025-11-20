module Korrvigs.Compute.Run where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Compute.Computation
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.Type
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Monad.Computation
import System.FilePath

resolveArg :: (MonadKorrvigs m) => FilePath -> RunArg -> m Text
resolveArg _ (ArgPlain txt) = pure txt
resolveArg tmpdir (ArgResult i cmp) =
  getComputation i cmp >>= \case
    Nothing -> pure "/dev/null"
    Just comp -> case comp ^. cmpResult of
      Nothing -> pure "/dev/null"
      -- TODO check hash and recursively run if necessary
      Just (_, _, result) -> do
        undefined
        let tp = comp ^. cmpRun . runType
        let filename = T.unpack $ unId i <> "_" <> cmp <> runTypeExt tp
        let path = tmpdir </> filename
        liftIO $ LBS.writeFile path $ encodeToLBS result
        pure $ T.pack path
resolveArg _ (ArgEntry i) =
  load i >>= \case
    Nothing -> pure "/dev/null"
    Just entry -> T.pack <$> entryFile entry
