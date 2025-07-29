module Korrvigs.File.New (new, NewFile (..), nfEntry, nfRemove, update) where

import Conduit (throwM)
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson (toJSON)
import Data.Aeson.Lens
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time.LocalTime
import qualified Korrvigs.Compute as Cpt
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.Mtdt
import Korrvigs.File.SQL
import Korrvigs.File.Sync
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Android
import Korrvigs.Monad
import Korrvigs.Monad.Metadata (listCompute)
import Korrvigs.Monad.Sync (syncFileOfKind)
import Korrvigs.Utils (joinNull, resolveSymbolicLink)
import Korrvigs.Utils.DateTree (FileContent (..), storeFile)
import Korrvigs.Utils.Git.Annex
import Korrvigs.Utils.Process
import Korrvigs.Utils.Time (dayToZonedTime)
import Network.Mime
import Opaleye hiding (not, null)
import System.Directory
import System.FilePath
import System.IO
import qualified System.Posix as Posix
import System.Process

splitLast :: (Eq a) => a -> [a] -> [a]
splitLast c' = either id (view _2) . foldr go (Left [])
  where
    go c (Right (f, b)) = Right (c : f, b)
    go c (Left s)
      | c == c' = Right ([], s)
      | otherwise = Left (c : s)

findMime :: FilePath -> IO MimeType
findMime path | takeExtension path == ".gpx" = pure "application/gpx+xml"
findMime path = do
  (_, Just out, _, _) <- createProcess file {std_out = CreatePipe}
  r <- hGetContents' out
  let mime = T.strip . T.pack $ splitLast ':' r
  pure $ Enc.encodeUtf8 mime
  where
    file = proc "file" ["--mime-type", path]

inAnnex :: (MonadKorrvigs m) => FilePath -> m Bool
inAnnex path = do
  korrRoot <- root
  gitRoot <- liftIO $ readCreateProcess ((proc "git" ["rev-parse", "--show-toplevel"]) {cwd = Just korrRoot}) ""
  pure $ isRelative $ makeRelative gitRoot path

shouldAnnex :: FilePath -> MimeType -> IO Bool
shouldAnnex path mime =
  if BS.isPrefixOf "text/" mime
    then do
      status <- Posix.getFileStatus path
      let size = fromIntegral $ Posix.fileSize status :: Integer
      pure $ size > 10 * 1024 * 1024
    else pure True

data NewFile = NewFile
  { _nfEntry :: NewEntry,
    _nfRemove :: Bool
  }

makeLenses ''NewFile

instance Default NewFile where
  def = NewFile def False

choosePrefix :: MimeType -> Text
choosePrefix mime
  | BS.isPrefixOf "audio" mime = "audio"
  | BS.isPrefixOf "video" mime = "vid"
  | BS.isPrefixOf "font" mime = "font"
  | BS.isPrefixOf "image" mime = "img"
  | BS.isPrefixOf "text" mime = "file"
  | otherwise = "doc"

applyNewOptions :: (MonadIO m) => NewEntry -> m (FileMetadata -> FileMetadata)
applyNewOptions ne = do
  dt <- mkdate
  pure $ foldr (.) id [parents, dt, title, lang, mtdt]
  where
    parents = exParents %~ (++ (ne ^. neParents))
    mkdate = do
      tz <- liftIO getCurrentTimeZone
      let dt = dayToZonedTime tz <$> ne ^. neDate
      pure $ maybe id (exDate ?~) dt
    title = maybe id ((annoted . at (mtdtSqlName Title) ?~) . toJSON) $ joinNull T.null $ ne ^. neTitle
    lang = maybe id ((annoted . at (mtdtSqlName Language) ?~) . toJSON) $ ne ^. neLanguage
    mtdt = annoted %~ unCIMtdt . useMtdt ne . reCIMtdt

update :: (MonadKorrvigs m) => File -> FilePath -> m ()
update file nfile = do
  let i = file ^. fileEntry . name
  -- Replace file
  let oldpath = file ^. filePath
  liftIO $ removeFile oldpath
  mime <- liftIO $ findMime nfile
  let mimeTxt = Enc.decodeUtf8 mime
  let ext = takeExtension nfile
  let newpath = replaceExtension oldpath ext
  liftIO $ copyFile nfile newpath
  annex <- liftIO $ shouldAnnex newpath mime
  rt <- root
  when annex $ annexAdd rt newpath
  -- Rename meta file
  let oldmeta = file ^. fileMeta
  let newmeta = addExtension (dropExtension (dropExtension oldmeta)) (ext <> ".meta")
  liftIO $ renameFile oldmeta newmeta
  -- Update SQL
  let status = if annex then FilePresent else FilePlain
  conn <- pgSQL
  liftIO $
    void $
      runUpdate conn $
        Update
          { uTable = filesTable,
            uUpdateWith =
              (sqlFilePath .~ sqlString newpath)
                . (sqlFileMeta .~ sqlString newmeta)
                . (sqlFileStatus .~ sqlFS status)
                . (sqlFileMime .~ sqlStrictText mimeTxt),
            uWhere = \row -> row ^. sqlFileName .== sqlId i,
            uReturning = rCount
          }

fromAndroid :: (MonadKorrvigs m) => (Text, FilePath) -> m (NewFile -> NewFile)
fromAndroid (adb, rel) = fmap (fromMaybe id) $ runMaybeT $ do
  let file = takeFileName rel
  phones <- lift listPhones
  phone <- hoistMaybe $ M.lookup adb phones
  let addPhone = M.insert (mtdtName FromAndroid) $ toJSON $ unId $ phone ^. androidEntry
  let addFile = M.insert (mtdtName FromAndroidPath) $ toJSON file
  pure $
    (nfEntry . neMtdt %~ addPhone . addFile)
      . (nfRemove .~ True)

new :: (MonadKorrvigs m) => FilePath -> NewFile -> m Id
new path' options' = do
  alreadyAnnexed <- inAnnex path'
  path <- liftIO $ resolveSymbolicLink path'
  isFromAndroid <- recogniseCaptured path
  options <- ($ options') <$> maybe (pure id) fromAndroid isFromAndroid
  ex <- liftIO $ doesFileExist path
  unless ex $ throwM $ KIOError $ userError $ "File \"" <> path <> "\" does not exists"
  mime <- liftIO $ findMime path
  let mimeTxt = Enc.decodeUtf8 mime
  let mtdt' = FileMetadata mimeTxt M.empty Nothing Nothing Nothing Nothing []
  mtdt'' <- liftIO $ ($ mtdt') <$> extractMetadata path mime
  mtdt <- ($ mtdt'') <$> applyNewOptions (options ^. nfEntry)
  annex <- liftIO $ shouldAnnex path mime
  let baseName = listToMaybe [T.pack (takeBaseName path') | null (options ^. nfEntry . neParents)]
  let idmk' =
        imk (choosePrefix mime)
          & idTitle
            .~ ( (mtdt ^? annoted . at (mtdtSqlName Title) . _Just . _String)
                   <|> baseName
               )
          & idDate .~ mtdt ^. exDate
  idmk <- applyNewEntry (options ^. nfEntry) idmk'
  i <- newId idmk
  let ext = T.pack $ takeExtension path
  let nm = unId i <> ext
  dir <- filesDirectory
  let day = localDay . zonedTimeToLocalTime <$> mtdt ^. exDate
  content <-
    if alreadyAnnexed
      then pure $ (if options ^. nfRemove then FileMove else FileCopy) path'
      else liftIO $ FileLazy <$> BSL.readFile path
  stored <- storeFile dir filesTreeType day nm content
  let metapath = metaPath stored
  liftIO $ TLIO.writeFile metapath $ encodeToLazyText mtdt
  rt <- root
  when (annex && not alreadyAnnexed) $ annexAdd rt stored
  when alreadyAnnexed $ void $ runSilentK (proc "git" ["annex", "fix", stored]) {cwd = Just rt}
  syncFileOfKind stored File
  when (options ^. nfRemove && not alreadyAnnexed) $ liftIO $ removeFile path'
  applyCollections (options ^. nfEntry) i
  comps <- listCompute i
  forM_ comps Cpt.run
  pure i
