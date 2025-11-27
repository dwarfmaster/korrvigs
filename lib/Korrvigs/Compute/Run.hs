module Korrvigs.Compute.Run where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import qualified Crypto.Hash as Hsh
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Text
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.SQL
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
runVeryLazy = runVeryLazy' S.empty

runVeryLazy' :: (MonadKorrvigs m) => Set (Id, Text) -> Computation -> m (Either Text RunnableResult)
runVeryLazy' seen cmp = case cmp ^. cmpResult of
  Nothing -> doRun' runVeryLazy' seen cmp
  Just (rtp, _, res) -> do
    let tp = cmp ^. cmpRun . runType
    if tp == rtp then pure (Right res) else doRun' runVeryLazy' seen cmp

runLazy :: (MonadKorrvigs m) => Computation -> m (Either Text RunnableResult)
runLazy = runLazy' S.empty

runLazy' :: (MonadKorrvigs m) => Set (Id, Text) -> Computation -> m (Either Text RunnableResult)
runLazy' seen cmp = case cmp ^. cmpResult of
  Nothing -> doRun' runLazy' seen cmp
  Just (_, rhsh, res) ->
    hashComputation cmp >>= \case
      Nothing -> throwM $ KMiscError $ "Failed to hash computation " <> cmpNm
      Just hsh -> if hsh == rhsh then pure (Right res) else doRun' runLazy' seen cmp
  where
    cmpNm = unId (cmp ^. cmpEntry) <> "#" <> cmp ^. cmpName

runForce :: (MonadKorrvigs m) => Computation -> m (Either Text RunnableResult)
runForce = runForce' S.empty

runForce' :: (MonadKorrvigs m) => Set (Id, Text) -> Computation -> m (Either Text RunnableResult)
runForce' = doRun' runForce'

doRun' ::
  (MonadKorrvigs m) =>
  (Set (Id, Text) -> Computation -> m (Either Text RunnableResult)) ->
  Set (Id, Text) ->
  Computation ->
  m (Either Text RunnableResult)
doRun' rec seen cmp =
  if alreadySeen
    then
      pure $
        Left $
          ("Circular dependencies between computations: " <>) $
            T.intercalate " <> " $
              (\(i, c) -> unId i <> "#" <> c) <$> S.toList seen
    else do
      (exit, res, stderr) <-
        runResourceT $
          runOut
            (cmp ^. cmpRun)
            (resolveArg $ rec nseen)
            (runOutputConduit $ runTypeKind $ cmp ^. cmpRun . runType)
            (decodeUtf8Lenient .| sinkLazy)
      case exit of
        ExitFailure i ->
          throwM $
            KMiscError $
              "Runnable failed with exit code "
                <> T.pack (show i)
                <> ": "
                <> LT.toStrict stderr
        ExitSuccess -> do
          forM_ res $ \r -> do
            mhsh <- hashComputation cmp
            forM_ mhsh $ \hsh -> do
              storeComputationResult
                (cmp ^. cmpEntry)
                (cmp ^. cmpName)
                (cmp ^. cmpRun . runType)
                hsh
                r
          pure res
  where
    current = (cmp ^. cmpEntry, cmp ^. cmpName)
    alreadySeen = S.member current seen
    nseen = S.insert current seen

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
