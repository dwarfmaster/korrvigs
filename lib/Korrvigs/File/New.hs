module Korrvigs.File.New (new, NewFile (..), nfEntry) where

import Conduit (throwM)
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.Mtdt
import Korrvigs.File.Sync
import Korrvigs.KindData
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Utils (resolveSymbolicLink)
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

new :: (MonadKorrvigs m) => FilePath -> NewFile -> m Id
new path' options = do
  path <- liftIO $ resolveSymbolicLink path'
  ex <- liftIO $ doesFileExist path
  unless ex $ throwM $ KIOError $ userError $ "File \"" <> path <> "\" does not exists"
  mime <- liftIO $ findMime path
  mtdt' <- liftIO $ extractMetadata path mime
  extrasFromNew <- newExtraMtdt $ options ^. nfEntry
  let mtdt'' = reifyMetadata extrasFromNew $ M.map (`MValue` True) mtdt'
  let mtdt = M.map (^. metaValue) mtdt''
  let extras = mtdtExtras mtdt
  annex <- liftIO $ shouldAnnex path mime
  let idmk' =
        imk (choosePrefix mime)
          & idTitle
            .~ ( (extras ^. mtdtTitle)
                   <|> Just (T.pack $ takeBaseName path)
               )
          & idDate .~ extras ^. mtdtDate
  idmk <- applyNewEntry (options ^. nfEntry) idmk'
  i <- newId idmk
  let ext = T.pack $ takeExtension path
  let nm = unId i <> ext
  content <- liftIO $ BSL.readFile path
  dir <- filesDirectory
  let day = (localDay . zonedTimeToLocalTime <$> extras ^. mtdtDate) <|> (options ^. nfEntry . neDate)
  stored <- storeFile dir filesTreeType day nm content
  let metapath = metaPath stored
  let meta =
        FileMetadata
          { _savedMime = Enc.decodeUtf8 mime,
            _extracted = mtdt,
            _annoted = M.fromList $ options ^. nfEntry . neMtdt
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
    devNull' <- openFile "/dev/null" WriteMode
    let gunstage =
          (proc "git" ["restore", "--staged", stored])
            { std_out = UseHandle devNull',
              std_err = UseHandle devNull',
              cwd = Just rt
            }
    (_, _, _, prcUnstage) <- createProcess gunstage
    void $ waitForProcess prcUnstage
    hClose devNull'
  relData <- dSyncOneImpl stored
  atomicInsertRelData i relData
  pure i
