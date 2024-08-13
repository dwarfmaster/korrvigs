module Korrvigs.File.New (new) where

import Control.Lens
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.File.Sync
import Korrvigs.KindData
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Utils.DateTree (storeFile)
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
findMime path = do
  (_, Just out, _, _) <- createProcess file {std_out = CreatePipe}
  r <- hGetContents' out
  let mime = T.strip . T.pack $ splitLast ':' r
  pure $ Enc.encodeUtf8 mime
  where
    file = proc "file" ["--mime-type", path]

extractMetadata :: FilePath -> MimeType -> IO (Map Text Value)
extractMetadata path mime = pure M.empty

shouldAnnex :: FilePath -> MimeType -> IO Bool
shouldAnnex path mime =
  if BS.isPrefixOf "text/" mime
    then do
      status <- Posix.getFileStatus path
      let size = fromIntegral $ Posix.fileSize status :: Integer
      pure $ size > 10 * 1024 * 1024
    else pure True

new :: (MonadKorrvigs m) => FilePath -> Maybe Entry -> m Id
new path parent = do
  ex <- liftIO $ doesFileExist path
  unless ex $ throwError $ KIOError $ userError $ "File \"" <> path <> "\" does not exists"
  mime <- liftIO $ findMime path
  mtdt <- liftIO $ extractMetadata path mime
  let extras = mtdtExtras mtdt
  annex <- liftIO $ shouldAnnex path mime
  let idmk =
        imk "file"
          & idTitle ?~ fromMaybe (T.pack $ takeBaseName path) (extras ^. mtdtTitle)
          & idParent .~ fmap (view name) parent
          & idDate .~ extras ^. mtdtDate
  i <- newId idmk
  let ext = T.pack $ takeExtension path
  let nm = unId i <> ext
  content <- liftIO $ BSL.readFile path
  dir <- filesDirectory
  stored <- storeFile dir filesTreeType (localDay . zonedTimeToLocalTime <$> extras ^. mtdtDate) nm content
  let metapath = metaPath stored
  let meta =
        FileMetadata
          { _savedMime = Enc.decodeUtf8 mime,
            _extracted = mtdt,
            _annoted = M.empty
          }
  liftIO $ TLIO.writeFile metapath $ encodeToLazyText meta
  rt <- root
  when annex $ liftIO $ do
    devNull <- openFile "/dev/null" WriteMode
    let gannex =
          (proc "git" ["annex", "add", stored])
            { std_out = UseHandle devNull,
              std_err = UseHandle devNull,
              cwd = Just rt
            }
    (_, _, _, prc) <- createProcess gannex
    void $ waitForProcess prc
    hClose devNull
  relData <- dSyncOneImpl stored
  atomicInsertRelData i relData
  pure i
