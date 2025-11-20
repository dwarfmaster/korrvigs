{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Compute.Computation where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Crypto.Hash as Hsh
import qualified Data.ByteString.Lazy as LBS
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.Type
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils.Crypto
import Korrvigs.Utils.Opaleye
import Korrvigs.Utils.Process
import Opaleye
import System.Exit
import System.Process

--   ____   ___  _
--  / ___| / _ \| |
--  \___ \| | | | |
--   ___) | |_| | |___
--  |____/ \__\_\_____|
--

data Computation = Computation
  { _cmpEntry :: Id,
    _cmpName :: Text,
    _cmpRun :: Runnable,
    _cmpResult :: Maybe (RunnableType, Hash, RunnableResult)
  }
  deriving (Eq, Show)

data CompRowImpl a b c = CompRow
  { _sqlCompEntry :: a,
    _sqlCompName :: b,
    _sqlCompHash :: c
  }

makeLenses ''Computation
makeLenses ''CompRowImpl
$(makeAdaptorAndInstanceInferrable "pCompRow" ''CompRowImpl)

type CompRow = CompRowImpl Int Text Text

type CompRowSQL = CompRowImpl (Field SqlInt4) (Field SqlText) (Field SqlCharN)

instance Default ToFields CompRow CompRowSQL where
  def = pCompRow $ CompRow def def def

computationsTable :: Table CompRowSQL CompRowSQL
computationsTable =
  table "computations" $
    pCompRow $
      CompRow
        (tableField "entry")
        (tableField "name")
        (tableField "hash")

selComp :: Field SqlInt4 -> Text -> Select CompRowSQL
selComp i nm = do
  cmp <- selectTable computationsTable
  where_ $ cmp ^. sqlCompEntry .== i
  where_ $ cmp ^. sqlCompName .== sqlStrictText nm
  pure cmp

getHash :: CompRow -> Maybe Hash
getHash = digestFromHexa . view sqlCompHash

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
  lift (rSelectOne sql) >>= \case
    Nothing -> fail $ "Computation " <> compName <> " does not exists"
    Just c -> maybe (fail $ "Invalid hash for " <> compName) pure $ getHash c
  where
    compName = T.unpack (unId i) <> "#" <> T.unpack cmp
    sql = do
      c <- selectTable computationsTable
      e <- selectTable entriesTable
      where_ $ e ^. sqlEntryName .== sqlId i
      where_ $ c ^. sqlCompEntry .== (e ^. sqlEntryId)
      where_ $ c ^. sqlCompName .== sqlStrictText cmp
      pure c

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

hashComputation :: (MonadKorrvigs m) => Computation -> m (Maybe Hash)
hashComputation = runMaybeT . hashRunnable getEntryHash getCompHash . view cmpRun
