module Korrvigs.Compute.Run where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Crypto.Hash as Hsh
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Compute.Computation
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.Type
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Monad.Computation
import Korrvigs.Utils.Process
import System.Exit
import System.FilePath
import System.Process

--   _   _           _
--  | | | | __ _ ___| |__
--  | |_| |/ _` / __| '_ \
--  |  _  | (_| \__ \ | | |
--  |_| |_|\__,_|___/_| |_|
--

entryFile :: (MonadKorrvigs m) => Entry -> m FilePath
entryFile entry = case entry ^. entryKindData of
  NoteD note -> pure $ note ^. notePath
  FileD file -> pure $ file ^. filePath
  EventD event -> pure $ event ^. eventFile
  CalendarD cal -> Cal.calendarPath cal
  SyndicateD syn -> pure $ syn ^. synPath

getCompHash ::
  (MonadKorrvigs m, MonadTrans t, MonadFail (t m)) =>
  Id ->
  Text ->
  t m Hash
getCompHash i cmp =
  lift (getComputation i cmp) >>= \case
    Nothing -> fail $ "Computation " <> compName <> " does not exists"
    Just c -> hashComputation' c
  where
    compName = T.unpack (unId i) <> "#" <> T.unpack cmp

getEntryHash ::
  (MonadKorrvigs m, MonadTrans t, MonadFail (t m)) =>
  Id ->
  t m Hash
getEntryHash i = do
  entry <- lift (load i) >>= maybe (fail $ "Couldn't load " <> T.unpack (unId i)) pure
  file <- lift $ entryFile entry
  rt <- lift root
  (exit, out) <- lift $ liftIO $ runStdout $ git rt ["status", "-s", file]
  lift $
    liftIO $
      fmap Hsh.hashlazy $
        if exit == ExitSuccess && LBS.null out
          then do
            (_, commitId) <- runStdout $ git rt ["rev-list", "-n1", "HEAD", "--", file]
            pure $ "commit:" <> commitId
          else ("bin:" <>) <$> LBS.readFile file
  where
    git rt args = (proc "git" args) {cwd = Just rt}

hashComputation' :: (MonadKorrvigs m, MonadTrans t, MonadFail (t m)) => Computation -> t m Hash
hashComputation' = hashRunnable getEntryHash getCompHash . view cmpRun

hashComputation :: (MonadKorrvigs m) => Computation -> m (Maybe Hash)
hashComputation = runMaybeT . hashComputation'

--   ____
--  |  _ \ _   _ _ __
--  | |_) | | | | '_ \
--  |  _ <| |_| | | | |
--  |_| \_\\__,_|_| |_|
--

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
