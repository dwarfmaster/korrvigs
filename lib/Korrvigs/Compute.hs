module Korrvigs.Compute where

import Conduit (throwM)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import qualified Crypto.Hash as Hash
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Actions.SQL
import qualified Korrvigs.Calendar.Sync as Cal
import Korrvigs.Compute.Action
import qualified Korrvigs.Compute.Builtin as Builtin
import Korrvigs.Compute.Declare
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils.Crypto
import Korrvigs.Utils.Git.Annex
import Korrvigs.Utils.Process
import Opaleye hiding (not)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

cacheDir :: (MonadKorrvigs m) => m FilePath
cacheDir = do
  rt <- root
  pure $ joinPath [rt, "cache"]

compFile' :: FilePath -> [CompHash] -> Action -> FilePath
compFile' dir deps act = joinPath [dir, T.unpack $ T.take 2 hash, T.unpack $ hash <> ext]
  where
    dat = actionData act
    ext :: Text
    ext = case dat ^. adatType of
      ScalarImage -> ".png"
      Picture -> ".jpg"
      VectorImage -> ".svg"
      Json -> ".json"
    hash = digestToHexa $ hashAction deps act

compFile :: (MonadKorrvigs m) => [CompHash] -> Action -> m FilePath
compFile deps act = do
  dir <- cacheDir
  pure $ compFile' dir deps act

-- Store data of a specific type into a cached data
storeCached' :: (MonadIO m) => FilePath -> CompType -> BSL.ByteString -> m (CompHash, FilePath)
storeCached' rt tp dat = do
  let hash = Hash.hashlazy dat
  let file = compFile' rt [] $ Cached tp hash
  let dir = takeDirectory file
  liftIO $ createDirectoryIfMissing True dir
  liftIO $ BSL.writeFile file dat
  pure (hash, file)

storeCachedJson' :: (MonadIO m, ToJSON val) => FilePath -> val -> m (CompHash, FilePath)
storeCachedJson' rt v =
  storeCached' rt Json $ LEnc.encodeUtf8 $ encodeToLazyText v

storeCached :: (MonadKorrvigs m) => CompType -> BSL.ByteString -> m (CompHash, FilePath)
storeCached tp dat = do
  rt <- cacheDir
  storeCached' rt tp dat

storeCachedJson :: (MonadKorrvigs m, ToJSON val) => val -> m (CompHash, FilePath)
storeCachedJson v = do
  rt <- cacheDir
  storeCachedJson' rt v

lookupComp :: (MonadKorrvigs m) => Id -> Text -> m (Maybe Action)
lookupComp i nm = rSelectOne $ do
  act <- selectTable computationsTable
  where_ $ act ^. sqlCompEntry .== sqlId i
  where_ $ act ^. sqlCompName .== sqlStrictText nm
  pure $ act ^. sqlCompAction

hashFile :: (MonadKorrvigs m) => FilePath -> m CompHash
hashFile path = do
  rt <- root
  annexed <- isAnnexedFile rt path
  if annexed
    then do
      tgt <- liftIO $ getSymbolicLinkTarget path
      pure $ Hash.hash $ Enc.encodeUtf8 $ T.pack $ "target:" <> tgt
    else do
      let gitSt = (proc "git" ["diff", "--quiet", "--exit-code", path]) {cwd = Just rt}
      let gitStStaged = (proc "git" ["diff", "--quiet", "--staged", "--exit-code", path]) {cwd = Just rt}
      st1 <- liftIO $ runSilent gitSt
      st2 <- liftIO $ runSilent gitStStaged
      if st1 == ExitSuccess && st2 == ExitSuccess
        then do
          let revList = (proc "git" ["rev-list", "-1", "HEAD", "--", path]) {cwd = Just rt}
          (_, ci) <- liftIO $ runStdout revList
          pure $ Hash.hashlazy $ "commit:" <> ci
        else do
          content <- liftIO $ LBS.readFile path
          pure $ Hash.hashlazy $ "content:" <> content

checkEntry :: (MonadKorrvigs m) => Id -> m CompHash
checkEntry i = do
  entry <- load i >>= throwMaybe (KMiscError $ "Failed to load " <> unId i)
  case entry ^. kindData of
    LinkD lnk -> hashFile $ lnk ^. linkPath
    NoteD note -> hashFile $ note ^. notePath
    EventD ev -> hashFile $ ev ^. eventFile
    CalendarD cal -> do
      path <- Cal.calendarPath cal
      hashFile path
    FileD file -> do
      hash1 <- hashFile $ file ^. filePath
      hash2 <- hashFile $ file ^. fileMeta
      let dat = digestToHexa hash1 <> ":" <> digestToHexa hash2
      pure $ Hash.hash $ Enc.encodeUtf8 dat

run' :: (MonadKorrvigs m) => FilePath -> Action -> m (CompHash, FilePath)
run' rt act = do
  comps <- forM (dat ^. adatDeps . depComps) $ \(i, nm) ->
    lookupComp i nm >>= \case
      Nothing -> throwM $ KMiscError $ "Failed to load computation " <> unId i <> "#" <> nm
      Just dact -> fst <$> run' rt dact
  entries <- forM (dat ^. adatDeps . depEntries) checkEntry
  let deps = comps ++ entries
  let hash = hashAction deps act
  let file = compFile' rt deps act
  ex <- liftIO $ doesFileExist file
  case (ex, act) of
    (True, _) -> pure ()
    (False, Builtin i blt) ->
      load i >>= \case
        Nothing -> throwM $ KMiscError $ "Failed to load " <> unId i <> " when executing action"
        Just entry -> do
          let dir = takeDirectory file
          liftIO $ createDirectoryIfMissing True dir
          Builtin.run blt entry file
    (False, Cached _ _) -> pure ()
  when (shouldAnnex && not ex) $ flip annexAdd file =<< root
  pure (hash, file)
  where
    dat = actionData act
    shouldAnnex :: Bool
    shouldAnnex = case dat ^. adatType of
      ScalarImage -> True
      Picture -> True
      VectorImage -> True
      Json -> False

run :: (MonadKorrvigs m) => Action -> m (CompHash, FilePath)
run act = cacheDir >>= \rt -> run' rt act

runJSON' :: (MonadKorrvigs m, FromJSON a) => FilePath -> Action -> m (Maybe a)
runJSON' rt act = do
  (_, file) <- run' rt act
  r <- liftIO $ eitherDecode <$> BSL.readFile file
  pure $ case r of
    Right v -> Just v
    Left _ -> Nothing

runJSON :: (MonadKorrvigs m, FromJSON a) => Action -> m (Maybe a)
runJSON act = cacheDir >>= \rt -> runJSON' rt act
