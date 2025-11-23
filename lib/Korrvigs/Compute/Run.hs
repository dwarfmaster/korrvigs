module Korrvigs.Compute.Run where

import Conduit
import Control.Lens
import Control.Monad.Trans.Maybe
import qualified Crypto.Hash as Hsh
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
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

resolveArg ::
  (MonadKorrvigs m, MonadTrans t, MonadIO (t m)) =>
  (Computation -> m (Either Text RunnableResult)) ->
  FilePath ->
  RunArg ->
  t m Text
resolveArg _ _ (ArgPlain txt) = pure txt
resolveArg runRec tmpdir (ArgResult i cmp) =
  lift (getComputation i cmp) >>= \case
    Nothing -> pure "/dev/null"
    Just comp ->
      lift (runRec comp) >>= \case
        Left _ -> pure "/dev/null"
        Right result -> do
          let tp = comp ^. cmpRun . runType
          let filename = T.unpack $ unId i <> "_" <> cmp <> runTypeExt tp
          let path = tmpdir </> filename
          liftIO $ LBS.writeFile path $ encodeToLBS result
          pure $ T.pack path
resolveArg _ _ (ArgEntry i) =
  lift $
    load i >>= \case
      Nothing -> pure "/dev/null"
      Just entry -> T.pack <$> entryFile entry

runVeryLazy :: (MonadKorrvigs m) => Computation -> m (Either Text RunnableResult)
runVeryLazy cmp = case cmp ^. cmpResult of
  Nothing -> doRun runVeryLazy cmp
  Just (rtp, _, res) -> do
    let tp = cmp ^. cmpRun . runType
    if tp == rtp then pure (Right res) else doRun runVeryLazy cmp

runLazy :: (MonadKorrvigs m) => Computation -> m (Either Text RunnableResult)
runLazy cmp = case cmp ^. cmpResult of
  Nothing -> doRun runLazy cmp
  Just (_, rhsh, res) ->
    hashComputation cmp >>= \case
      Nothing -> throwM $ KMiscError $ "Failed to hash computation " <> cmpNm
      Just hsh -> if hsh == rhsh then pure (Right res) else doRun runLazy cmp
  where
    cmpNm = unId (cmp ^. cmpEntry) <> "#" <> cmp ^. cmpName

runForce :: (MonadKorrvigs m) => Computation -> m (Either Text RunnableResult)
runForce = doRun runForce

doRun ::
  (MonadKorrvigs m) =>
  (Computation -> m (Either Text RunnableResult)) ->
  Computation ->
  m (Either Text RunnableResult)
doRun rec cmp = do
  (exit, res) <-
    runResourceT $
      runOut
        (cmp ^. cmpRun)
        (resolveArg rec)
        (runOutputConduit $ runTypeKind $ cmp ^. cmpRun . runType)
  case exit of
    ExitFailure i ->
      throwM $ KMiscError $ "Runnable failed with exit code " <> T.pack (show i)
    ExitSuccess -> pure res

runOutputConduit :: (MonadKorrvigs m) => RunnableKind -> ConduitT ByteString Void (ResourceT m) (Either Text RunnableResult)
runOutputConduit KindBin = Right . ResultBinary <$> sinkLazy
runOutputConduit KindText =
  Right . ResultText . LT.toStrict <$> (decodeUtf8Lenient .| sinkLazy)
runOutputConduit KindJson = dec <$> sinkLazy
  where
    dec =
      mapLeft T.pack
        . fmap ResultJson
        . eitherDecode
    mapLeft _ (Right x) = Right x
    mapLeft f (Left x) = Left $ f x
