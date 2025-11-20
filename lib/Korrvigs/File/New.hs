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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
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
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Monad.Metadata (listCompute)
import Korrvigs.Monad.Sync (syncFileOfKind)
import Korrvigs.Utils (joinNull, resolveSymbolicLink)
import Korrvigs.Utils.DateTree (FileContent (..), storeFile)
import Korrvigs.Utils.Process
import Network.HTTP.Conduit hiding (path)
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Status
import Network.Mime
import Network.URI hiding (path)
import Opaleye hiding (not, null)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import qualified System.Posix as Posix
import System.Process
import Text.Parsec hiding ((<|>))

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

data NewDownloadedFile = NewDownloadedFile
  { _ndlUrl :: Text,
    _ndlEntry :: NewEntry
  }

makeLenses ''NewDownloadedFile

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
  dt <- useDate ne Nothing
  pure $ foldr (.) id [parents, maybe id (exDate ?~) dt, title, lang, mtdt]
  where
    parents = exParents %~ (++ (ne ^. neParents))
    title = maybe id (exTitle ?~) $ joinNull T.null $ ne ^. neTitle
    lang = maybe id ((annoted . at (mtdtSqlName Language) ?~) . toJSON) $ ne ^. neLanguage
    mtdt = annoted %~ unCIMtdt . useMtdt ne . reCIMtdt

update :: (MonadKorrvigs m) => File -> FilePath -> m ()
update file nfile = do
  let i = file ^. fileEntry . entryId
  -- Replace file
  let oldpath = file ^. filePath
  liftIO $ removeFile oldpath
  mime <- liftIO $ findMime nfile
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
  let basename = listToMaybe [T.pack (takeBaseName path') | null (options ^. nfEntry . neParents)]
  let title = joinNull T.null (options ^. nfEntry . neTitle) <|> basename
  nentry <- applyCover (options ^. nfEntry) title
  ex <- liftIO $ doesFileExist path
  unless ex $ throwM $ KIOError $ userError $ "File \"" <> path <> "\" does not exists"
  mime <- liftIO $ findMime path
  let mimeTxt = Enc.decodeUtf8 mime
  let mtdt' = FileMetadata mimeTxt M.empty Nothing Nothing Nothing Nothing title [] M.empty
  mtdt'' <- liftIO $ ($ mtdt') <$> extractMetadata path mime
  mtdt <- ($ mtdt'') <$> applyNewOptions nentry
  let idmk' =
        imk (choosePrefix mime)
          & idTitle .~ title
          & idDate .~ mtdt ^. exDate
  idmk <- applyNewEntry nentry idmk'
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
  liftIO $ BSL.writeFile metapath $ encodePretty mtdt
  rt <- root
  when alreadyAnnexed $ void $ runSilentK (proc "git" ["annex", "fix", stored]) {cwd = Just rt}
  syncFileOfKind stored File
  when (options ^. nfRemove && not alreadyAnnexed) $ liftIO $ removeFile path'
  applyOnNewEntry nentry i
  comps <- listCompute i
  forM_ comps Cpt.run
  pure i

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

newFromUrl :: (MonadKorrvigs m) => NewDownloadedFile -> m (Maybe Id)
newFromUrl dl = do
  man <- manager
  withRunInIO $ \runIO ->
    withSystemTempDirectory "korrvigsDownload" $ \dir -> do
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
        Nothing -> pure Nothing
        Just tmp -> do
          let nfile = NewFile (dl ^. ndlEntry) False & nfEntry . neMtdt %~ M.insert (mtdtName Url) (toJSON url)
          i <- runIO $ new tmp nfile
          pure $ Just i

applyCover :: (MonadKorrvigs m) => NewEntry -> Maybe Text -> m NewEntry
applyCover ne title = do
  mne <- fmap join $ forM (ne ^. neCover) $ \cover -> do
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
  pure $ fromMaybe ne mne
