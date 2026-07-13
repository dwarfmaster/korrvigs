module Korrvigs.Cli.Blog where

import Conduit
import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.Trans.Reader
import qualified Data.Binary.Builder as Bld
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Process
import Data.Conduit.Text
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO8
import Korrvigs.Calendar.Sync (calendarPath)
import Korrvigs.Cli.Monad
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Metadata.Blog
import Korrvigs.Monad
import Korrvigs.Monad.Computation
import Korrvigs.Note
import Korrvigs.Note.AST
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Blaze.Html5 (Html)

data Cmd = Publish {_pubDir :: FilePath, _pubWebRoot :: Text, _pubEntry :: Text}

makePrisms ''Cmd
makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "publish"
      ( info
          ( ( Publish
                <$> argument str (metavar "DIR" <> help "Directory to publish the blog in.")
                <*> argument str (metavar "URL" <> help "URL the blog will be available from")
                <*> (fromMaybe "Blog" <$> optional (option str $ long "entry" <> help "The entry to generate the blog from"))
            )
              <**> helper
          )
          ( progDesc "Export blog to directory from metadata"
              <> header "korr blog publish -- publish blog as static files"
          )
      )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with blog export"
      <> header "korr blog -- blog export"

data BlogWriter = BlogWriter
  { _bgDir :: FilePath,
    _bgConfig :: BlogConfig,
    _bgStructure :: BlogStructure
  }

makeLenses ''BlogWriter

type BgWriter = ReaderT BlogWriter KorrM

clearDirectoryContent :: FilePath -> IO ()
clearDirectoryContent dir =
  listDirectory dir
    >>= mapM_
      ( \file -> do
          let path = dir </> file
          isDir <- doesDirectoryExist path
          if isDir
            then removeDirectoryRecursive path
            else removeFile path
      )

run :: Cmd -> KorrM ()
run (Publish dir url entry) = do
  liftIO $ clearDirectoryContent dir
  liftIO $ createDirectoryIfMissing True dir
  let cfg =
        BlogConfig
          { _blogCfgUrl = url,
            _blogCfgNote = MkId entry,
            _blogCfgOnlyPublished = True
          }
  bgStr <- loadStructure cfg
  let st = BlogWriter dir cfg bgStr
  runReaderT writeBlog st

writeBlog :: BgWriter ()
writeBlog = do
  bgStr <- view bgStructure
  forM_ (M.toList $ bgStr ^. blogFiles) $ uncurry writeBlogFile

writeBlogFile :: BlogUrl -> BlogContent -> BgWriter ()
writeBlogFile url content = do
  dir <- view bgDir
  path <- lift $ T.unpack <$> renderUrl False dir url
  liftIO $ createDirectoryIfMissing True $ takeDirectory path
  writeContent path url content

renderUrl :: Bool -> FilePath -> BlogUrl -> KorrM Text
renderUrl isLink dir url = do
  pure $ T.pack $ (dir </>) $ renderPath url
  where
    addIndex = if isLink then id else (</> "index.html")
    renderPath (BlogTopLevel txt) = T.unpack txt
    renderPath (BlogFilePlain file) = "files" </> T.unpack file
    renderPath (BlogPostNote note) = addIndex $ "posts" </> T.unpack note
    renderPath (BlogComputation entry cmp tp) = "posts" </> T.unpack entry </> T.unpack (cmp <> "." <> runTypeExt tp)
    renderPath BlogArchive = addIndex "archive"
    renderPath (BlogArchiveTag tag) = "archive" </> (T.unpack $ tag <> ".html")
    renderPath BlogAtom = "atom.xml"
    renderPath (BlogAtomTag tag) = T.unpack $ "atom-" <> tag <> ".xml"

topEntries :: BlogStructure -> Id -> KorrM (Maybe BlogUrl)
topEntries bgStr tgt = do
  let topFile = find ((== BlogFromNote tgt) . snd) $ M.toList $ bgStr ^. blogFiles
  pure $ fst <$> topFile

writeHtml :: (MonadIO m) => FilePath -> Html -> m ()
writeHtml path html =
  liftIO $ LBS.writeFile path $ Bld.toLazyByteString $ renderHtmlBuilder html

writeContent :: FilePath -> BlogUrl -> BlogContent -> BgWriter ()
writeContent path url (BlogFromNote i) = do
  bgStr <- view bgStructure
  mtdt <- view $ bgStructure . blogMtdt
  dir <- view $ bgConfig . blogCfgUrl . to T.unpack
  html <- lift $ renderPost url (bgStr ^. blogMenu) (Just $ bgStr ^. blogCSL) (renderUrl True dir) (topEntries bgStr) mtdt i
  writeHtml path html
writeContent path _ (BlogFromFile i) = do
  entry <- lift $ load i >>= throwMaybe (KCantLoad i "Load file for blog export")
  entryPth <- lift $ entryPath entry
  liftIO $ copyFile entryPth path
writeContent path _ (BlogFromCode i cd) = do
  entry <- lift $ load i >>= throwMaybe (KCantLoad i "Load code for blog export")
  note <- lift $ throwMaybe (KMiscError $ "During blog export, expected " <> unId i <> " to be a note") $ entry ^? _Note
  doc <- lift $ readNote (note ^. notePath) >>= throwEither (\e -> KMiscError $ "During blog export, failed to parse note " <> unId i <> ": " <> e)
  (attr, content) <- lift $ throwMaybe (KMiscError $ "During blog export, " <> cd <> " is not a code block of " <> unId i) $ doc ^? docContent . each . bkNamedCode cd
  liftIO $
    if "css" `elem` (attr ^. attrClasses)
      then yuiCompress "css" path content
      else
        if "js" `elem` (attr ^. attrClasses)
          then yuiCompress "js" path content
          else TIO8.writeFile path content
writeContent path _ (BlogFromComp i cmp) = do
  comp <- lift $ getComputation i cmp >>= throwMaybe (KMiscError $ "During blog export, couldn't find " <> unId i <> "#" <> cmp)
  r <- lift $ runLazy comp
  liftIO $ LBS.writeFile path $ encodeToLBS r
writeContent path _ BlogFromArchive = do
  dir <- view $ bgConfig . blogCfgUrl . to T.unpack
  onlyPublished <- view $ bgConfig . blogCfgOnlyPublished
  bgStr <- view bgStructure
  tags <- view $ bgStructure . blogTags
  mtdt <- view $ bgStructure . blogMtdt
  html <- lift $ generateArchivePage mtdt onlyPublished (bgStr ^. blogMenu) (renderUrl True dir) Nothing tags
  writeHtml path html
writeContent path _ (BlogFromArchiveTag tag) = do
  dir <- view $ bgConfig . blogCfgUrl . to T.unpack
  onlyPublished <- view $ bgConfig . blogCfgOnlyPublished
  bgStr <- view bgStructure
  mtdt <- view $ bgStructure . blogMtdt
  html <- lift $ generateArchivePage mtdt onlyPublished (bgStr ^. blogMenu) (renderUrl True dir) (Just tag) []
  writeHtml path html
writeContent path _ BlogFromAtom = do
  bgStr <- view bgStructure
  dir <- view $ bgConfig . blogCfgUrl . to T.unpack
  onlyPublished <- view $ bgConfig . blogCfgOnlyPublished
  atom <- lift $ renderAtom onlyPublished (renderUrl True dir) (topEntries bgStr) Nothing "Blog feed"
  liftIO $ LBS.writeFile path atom
writeContent path _ (BlogFromAtomTag tag) = do
  bgStr <- view bgStructure
  dir <- view $ bgConfig . blogCfgUrl . to T.unpack
  onlyPublished <- view $ bgConfig . blogCfgOnlyPublished
  atom <- lift $ renderAtom onlyPublished (renderUrl True dir) (topEntries bgStr) (Just tag) $ "Blog feed for " <> tag
  liftIO $ LBS.writeFile path atom

entryPath :: Entry -> KorrM FilePath
entryPath entry = case entry ^. entryKindData of
  FileD file -> pure $ file ^. filePath
  NoteD note -> pure $ note ^. notePath
  EventD ev -> pure $ ev ^. eventFile
  CalendarD cal -> calendarPath cal
  SyndicateD syn -> pure $ syn ^. synPath

yuiCompress :: Text -> FilePath -> Text -> IO ()
yuiCompress tp tgt content = do
  let yui = proc "yuicompressor" ["--type", T.unpack tp]
  (exit, (), ()) <-
    runResourceT $
      sourceProcessWithStreams
        yui
        (yield content .| encode utf8)
        (sinkFile tgt)
        (sinkFile "/dev/null")
  when (exit /= ExitSuccess) $ throwM $ KMiscError $ "Failed to compress " <> T.pack tgt
