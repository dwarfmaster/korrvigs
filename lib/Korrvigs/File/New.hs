module Korrvigs.File.New
  ( new,
    NewFile (..),
    nfEntry,
    nfRemove,
    newFromUrl,
    NewDownloadedFile (..),
    ndlUrl,
    ndlEntry,
    update,
    applyCover,
    moveFile,
  )
where

import Conduit
import Control.Applicative ((<|>))
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Aeson (toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Default
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Time.LocalTime
import Korrvigs.Compute.SQL
import Korrvigs.Entry
import qualified Korrvigs.Entry.JSON as Gen
import Korrvigs.Entry.New
import Korrvigs.File.Mtdt
import Korrvigs.File.SQL
import Korrvigs.File.Sync
import Korrvigs.Kind
import Korrvigs.Log hiding (logError)
import Korrvigs.Metadata
import Korrvigs.Metadata.Android
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import qualified Korrvigs.Monad.Computation as Comp
import Korrvigs.Monad.Sync (syncFileOfKind)
import qualified Korrvigs.Note.Download as Dl
import Korrvigs.Utils
import Korrvigs.Utils.DateTree (FileContent (..), storeFile)
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Process
import Network.HTTP.Conduit hiding (path)
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Status
import Network.Mime
import Network.URI hiding (path)
import Opaleye hiding (not, null)
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import qualified System.Posix as Posix
import System.Process
import Text.HTML.TagSoup
import Text.Parsec hiding ((<|>))
import Prelude hiding (log)

findMime :: FilePath -> FilePath -> IO MimeType
findMime _ path | takeExtension path == ".gpx" = pure "application/gpx+xml"
findMime _ path | takeExtension path == ".slvs" = pure "application/x-solvespace"
findMime _ path | takeExtension path == ".vik" = pure "application/x-viking"
findMime db path = do
  (exit, out) <- runStdout file
  case exit of
    ExitSuccess -> pure ()
    ExitFailure exitCode -> throwM $ KMiscError $ "Couldn't computate mime for file, mimetype failed with exit code " <> T.pack (show exitCode) <> ":\n" <> T.pack path
  let mime = T.strip $ LT.toStrict $ LEnc.decodeUtf8 out
  pure $ Enc.encodeUtf8 mime
  where
    file = proc "mimetype" ["--database", db, "--output-format", "%m", path]

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

data NewDownloadedFile = NewDownloadedFile
  { _ndlUrl :: Text,
    _ndlEntry :: NewEntry
  }

makeLenses ''NewDownloadedFile

applyNewOptions :: (MonadIO m) => NewEntry -> m (FileMetadata -> FileMetadata)
applyNewOptions ne = do
  dt <- useDate ne Nothing
  pure $ foldr (.) id [parents, maybe id (genData . Gen.ejsDate ?~) dt, title, lang, mtdt]
  where
    parents = genData . Gen.ejsParents %~ (++ (unId <$> ne ^. neParents))
    title = maybe id (genData . Gen.ejsTitle ?~) $ joinNull T.null $ ne ^. neTitle
    lang = maybe id ((genData . Gen.ejsMetadata . at (mtdtSqlName Language) ?~) . toJSON) $ ne ^. neLanguage
    mtdt = genData . Gen.ejsMetadata %~ unCIMtdt . useMtdt ne . reCIMtdt

update :: (MonadKorrvigs m) => File -> FilePath -> m ()
update file nfile = do
  let i = file ^. fileEntry . entryId
  -- Replace file
  let oldpath = file ^. filePath
  liftIO $ removeFile oldpath
  db <- mimeDatabase
  mime <- liftIO $ findMime db nfile
  let mimeTxt = Enc.decodeUtf8 mime
  let ext = takeExtension nfile
  let newpath = replaceExtension oldpath ext
  liftIO $ copyFile nfile newpath
  annex <- liftIO $ shouldAnnex newpath mime
  -- Rename meta file
  let oldmeta = file ^. fileMeta
  let newmeta = addExtension (dropExtension (dropExtension oldmeta)) (ext <> ".meta")
  liftIO $ renameFile oldmeta newmeta
  -- Update SQL
  let status = if annex then FilePresent else FilePlain
  withSQL $ \conn ->
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
              uWhere = \row -> row ^. sqlFileId .== sqlInt4 i,
              uReturning = rCount
            }
  -- Clear cached computations
  cmps <- rSelect $ do
    cmp <- selectTable computationsTable
    where_ $ cmp ^. sqlCompEntry .== sqlInt4 i
    pure $ cmp ^. sqlCompName
  Comp.clearComputationsResult (file ^. fileEntry) cmps

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

isDevicePresent :: (MonadKorrvigs m) => Text -> ZonedTime -> m (Maybe Id)
isDevicePresent dev dt = rSelectOne $ do
  devM <- selectTable entriesMetadataTable
  where_ $ devM ^. sqlKey .== sqlStrictText (mtdtSqlName Device)
  where_ $ devM ^. sqlValue .== sqlValueJSONB dev
  entry <- selectTable entriesTable
  where_ $ entry ^. sqlEntryId .== (devM ^. sqlEntry)
  where_ $ matchNullable (sqlBool False) (.== sqlZonedTime dt) (entry ^. sqlEntryDate)
  pure $ entry ^. sqlEntryName

isAlreadyPresent :: (MonadKorrvigs m) => FileMetadata -> m (Maybe Id)
isAlreadyPresent mtdt = runMaybeT $ do
  dev <- hoistMaybe $ mtdt ^? genData . Gen.ejsMetadata . at (mtdtSqlName Device) . _Just . to fromJSONM . _Just
  dt <- hoistMaybe $ mtdt ^. genData . Gen.ejsDate
  hoistLift $ isDevicePresent dev dt

new :: (MonadKorrvigs m) => FilePath -> NewFile -> m Id
new path' options' = $withLogContext ("Creating new file from " <> T.pack path') $ do
  alreadyAnnexed <- inAnnex path'
  path <- liftIO $ resolveSymbolicLink path'
  $logTrace $ MiscEvent $ "Resolved path: " <> T.pack path
  isFromAndroid <- recogniseCaptured path
  $logTrace $ MiscEvent $ "Already annexed: " <> T.pack (show alreadyAnnexed)
  $logTrace $ MiscEvent $ "From Android: " <> T.pack (show isFromAndroid)
  options <- ($ options') <$> maybe (pure id) fromAndroid isFromAndroid
  let basename = listToMaybe [T.pack (takeBaseName path') | null (options ^. nfEntry . neParents)]
  ex <- liftIO $ doesFileExist path
  $logError $ MiscEvent $ "File does not exists"
  unless ex $ throwM $ KIOError $ userError $ "File \"" <> path <> "\" does not exists"
  db <- mimeDatabase
  mime <- liftIO $ findMime db path
  let mimeTxt = Enc.decodeUtf8 mime
  $logTrace $ MiscEvent $ "Found mime: " <> mimeTxt
  let mtdt' = FileMetadata mimeTxt (def & Gen.ejsTitle .~ basename) M.empty
  mtdt'' <- liftIO $ ($ mtdt') <$> extractMetadata path mime
  let title = mtdt'' ^. genData . Gen.ejsTitle <|> joinNull T.null (options ^. nfEntry . neTitle)
  forM_ title $ \t -> $logTrace $ MiscEvent $ "Title: \"" <> t <> "\""
  nentry <- applyCover (options ^. nfEntry) title
  mtdt <- ($ mtdt'') <$> applyNewOptions nentry
  alreadyPresent <- isAlreadyPresent mtdt
  case alreadyPresent of
    Just i -> do
      $log $ EntryAlreadyExistsEvent File i
      pure i
    Nothing -> do
      let idmk' =
            imk (choosePrefix $ PrefixFile mime)
              & idTitle .~ title
              & idDate .~ mtdt ^. genData . Gen.ejsDate
      idmk <- applyNewEntry nentry idmk'
      i <- newId idmk
      $log $ NewEntryEvent File i
      let ext = T.pack $ takeExtension path
      let nm = unId i <> ext
      dir <- filesDirectory
      let day = localDay . zonedTimeToLocalTime <$> mtdt ^. genData . Gen.ejsDate
      content <-
        if alreadyAnnexed
          then pure $ (if options ^. nfRemove then FileMove else FileCopy) path'
          else liftIO $ FileLazy <$> BSL.readFile path
      stored <- storeFile dir filesTreeType day nm content
      $logTrace $ MiscEvent $ "Storing to " <> T.pack stored
      let metapath = metaPath stored
      $logTrace $ MiscEvent $ "Writing metadata to " <> T.pack metapath
      liftIO $ BSL.writeFile metapath $ encodePretty mtdt
      rt <- root
      when alreadyAnnexed $ void $ runSilentK (proc "git" ["annex", "fix", stored]) {cwd = Just rt}
      sqlI <- insertNew i File
      syncFileOfKind i stored sqlI File
      when (options ^. nfRemove && not alreadyAnnexed) $ liftIO $ removeFile path'
      applyOnNewEntry nentry i
      pure i

-- Moves a file already in the annex to a new emplacement determined by new ID
moveFile :: (MonadKorrvigs m) => File -> Id -> m ()
moveFile file ni = do
  let path = file ^. filePath
  let meta = file ^. fileMeta
  let day = localDay . zonedTimeToLocalTime <$> file ^. fileEntry . entryDate
  let nm = unId ni <> T.pack (takeExtension path)
  dir <- filesDirectory
  newPath <- storeFile dir filesTreeType day nm (FileMove path)
  let newMeta = metaPath newPath
  liftIO $ BSL.writeFile newMeta =<< BSL.readFile meta
  recursiveRemoveFile dir meta
  void $ atomicSQL $ \conn ->
    runUpdate conn $
      Update
        { uTable = filesTable,
          uUpdateWith =
            (sqlFilePath .~ sqlString newPath)
              . (sqlFileMeta .~ sqlString newMeta),
          uWhere = \f -> f ^. sqlFileId .== sqlInt4 (file ^. fileEntry . entryId),
          uReturning = rCount
        }

fileNameP :: Parsec ByteString () (Maybe (Bool, FilePath))
fileNameP = do
  spaces
  nm <- many1 $ satisfy (\c -> c == '*' || c == '-' || isLetter c)
  void $ char '='
  case nm of
    "filename*" -> do
      void $ string "UTF-8'"
      void $ many $ noneOf "'"
      void $ char '\''
      s <- many1 $ noneOf [';']
      let file = Enc.decodeUtf8 $ H.urlDecode True $ Enc.encodeUtf8 $ T.pack s
      pure $ Just (False, T.unpack file)
    "filename" -> Just . (True,) <$> (quoted <|> plain)
    _ -> do
      void $ many $ noneOf [';']
      pure Nothing
  where
    quoted = do
      void $ char '"'
      s <- many1 $ noneOf ['"']
      void $ char '"'
      pure s
    plain = many1 $ noneOf [';']

contDispP :: Parsec ByteString () (Maybe FilePath)
contDispP = do
  void $ many $ noneOf [';']
  option Nothing $ do
    void $ char ';'
    files <- catMaybes <$> sepBy fileNameP (char ';')
    case sort files of
      ((_, file) : _) -> pure $ Just file
      _ -> pure Nothing

contDispGetFilename :: ByteString -> Maybe FilePath
contDispGetFilename bs = case runParser contDispP () "content-disposition" bs of
  Left _ -> Nothing
  Right v -> v

extractOgImage :: Tag Text -> Endo NewDownloadedFile
extractOgImage tag@(TagOpen "meta" attrs)
  | tag ~== TagOpen ("meta" :: Text) [("property", "og:image")] =
      case lookup "content" attrs of
        Nothing -> mempty
        Just img -> Endo $ ndlUrl .~ img
extractOgImage _ = mempty

extractDeviantArt :: (MonadKorrvigs m) => Text -> [Tag Text] -> m (Endo NewDownloadedFile)
extractDeviantArt url tags
  | "https://www.deviantart.com" `T.isPrefixOf` url =
      pure $ foldMap extractOgImage tags
  where

extractDeviantArt _ _ = pure mempty

extractKonachan :: (MonadKorrvigs m) => Text -> [Tag Text] -> m (Endo NewDownloadedFile)
extractKonachan url tags
  | "https://konachan.com" `T.isPrefixOf` url =
      pure $ foldMap extractOgImage tags
extractKonachan _ _ = pure mempty

extractFromUrl :: (MonadKorrvigs m) => Text -> m (Endo NewDownloadedFile)
extractFromUrl url = do
  endo <- Dl.downloadInformationWithExtractor ndlEntry [extractDeviantArt, extractKonachan] url
  pure $
    mconcat
      [ endo,
        Endo (ndlEntry . neMtdt . at (mtdtName Url) %~ Just . fromMaybe (toJSON url))
      ]

newFromUrl :: (MonadKorrvigs m) => NewDownloadedFile -> m (Maybe Id)
newFromUrl dl' = $(withLogContext) ("New file from url " <> dl' ^. ndlUrl) $ do
  dlEndo <- extractFromUrl $ dl' ^. ndlUrl
  let dl = appEndo dlEndo dl'
  $(logTrace) $ MiscEvent $ "Effective url " <> dl ^. ndlUrl
  man <- manager
  withRunInIO $ \runIO ->
    withSystemTempDirectory "korrvigsDownload" $ \dir -> do
      runIO $ $(logTrace) $ MiscEvent $ "Downloading to " <> T.pack dir
      let url = dl ^. ndlUrl
      let urlFileName = takeFileName . uriPath <$> parseURI (T.unpack url)
      req <- parseRequest $ T.unpack $ dl ^. ndlUrl
      success <- runResourceT $ do
        resp <- http req man
        let contentDisposition = find ((== "content-disposition") . fst) $ responseHeaders resp
        let hdFileName = contDispGetFilename . snd =<< contentDisposition
        let fileName = fromMaybe "download" $ hdFileName <|> urlFileName
        let tmp = joinPath [dir, fileName]
        let scode = statusCode (responseStatus resp)
        if scode == 200
          then runConduit (responseBody resp .| sinkFile tmp) >> pure (Just tmp)
          else pure Nothing
      case success of
        Nothing -> do
          runIO $ $(logTrace) $ MiscEvent "Failed to download url"
          pure Nothing
        Just tmp -> do
          let nfile = NewFile (dl ^. ndlEntry) False & nfEntry . neCover .~ Nothing
          i <- runIO $ new tmp nfile
          pure $ Just i

applyCover :: (MonadKorrvigs m) => NewEntry -> Maybe Text -> m NewEntry
applyCover ne title = do
  mne <- fmap join $ forM (ne ^. neCover) $ \cover -> $withLogContext ("Applying cover " <> cover) $ do
    let nw =
          NewDownloadedFile cover $
            def
              & neTitle .~ fmap (<> " cover") title
              & neInhibitCapture .~ True
    mcovId <- newFromUrl nw
    forM mcovId $ \covId ->
      pure $
        ne
          & neMtdt . at (mtdtName Cover) ?~ toJSON (unId covId)
          & neChildren %~ (covId :)
  let rne = fromMaybe ne mne
  reifyNew rne
