module Korrvigs.File.New (new, NewFile (..), nfEntry) where

import Conduit (throwM)
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
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
import Korrvigs.Actions
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.Mtdt
import Korrvigs.File.Sync
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Utils (joinNull, resolveSymbolicLink)
import Korrvigs.Utils.DateTree (FileContent (..), storeFile)
import Korrvigs.Utils.Git.Annex
import Korrvigs.Utils.Time (dayToZonedTime)
import Network.Mime
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

shouldAnnex :: FilePath -> MimeType -> IO Bool
shouldAnnex path mime =
  if BS.isPrefixOf "text/" mime
    then do
      status <- Posix.getFileStatus path
      let size = fromIntegral $ Posix.fileSize status :: Integer
      pure $ size > 10 * 1024 * 1024
    else pure True

newtype NewFile = NewFile
  { _nfEntry :: NewEntry
  }

makeLenses ''NewFile

instance Default NewFile where
  def = NewFile def

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
    mtdt = annoted %~ M.union (M.fromList $ ne ^. neMtdt)

new :: (MonadKorrvigs m) => FilePath -> NewFile -> m Id
new path' options = do
  path <- liftIO $ resolveSymbolicLink path'
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
  content <- liftIO $ BSL.readFile path
  dir <- filesDirectory
  let day = localDay . zonedTimeToLocalTime <$> mtdt ^. exDate
  stored <- storeFile dir filesTreeType day nm $ FileLazy content
  let metapath = metaPath stored
  liftIO $ TLIO.writeFile metapath $ encodeToLazyText mtdt
  rt <- root
  when annex $ annexAdd rt stored
  syncFileOfKind stored File
  pure i
