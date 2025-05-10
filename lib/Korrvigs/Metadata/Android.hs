module Korrvigs.Metadata.Android where

import Control.Arrow
import Control.Lens hiding (from, ignored)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Metadata
import qualified Korrvigs.Metadata.Android.ADB as ADB
import Korrvigs.Metadata.TH
import Korrvigs.Monad
import Korrvigs.Utils.JSON
import Opaleye
import System.Directory
import System.FilePath

mkMtdt "Adb" "adb" [t|Text|]
mkMtdt "AndroidWatching" "adbwatched" [t|[Text]|]
mkMtdt "AndroidIgnored" "adbignored" [t|[Text]|]
mkMtdt "FromAndroid" "androidphone" [t|Text|]
mkMtdt "FromAndroidPath" "androidpath" [t|Text|]

data AndroidPhone = AndroidPhone
  { _androidEntry :: Id,
    _androidAdb :: Text,
    _androidIgnored :: Set Text,
    _androidWatched :: [Text]
  }
  deriving (Eq, Ord, Show)

makeLenses ''AndroidPhone

-- List only phones with at least one watched directory
listPhones :: (MonadKorrvigs m) => m (Map Text AndroidPhone)
listPhones = do
  phonesSQL <- rSelect $ do
    adb <- selectTable entriesMetadataTable
    where_ $ adb ^. sqlKey .== sqlStrictText (mtdtSqlName Adb)
    watching <- selectMtdt AndroidWatching $ adb ^. sqlEntry
    ignored <- selectMtdt AndroidIgnored $ adb ^. sqlEntry
    pure (adb ^. sqlEntry, adb ^. sqlValue, watching, ignored)
  let phones = mapMaybe extractPhone phonesSQL
  pure $ M.fromList $ (view androidAdb &&& id) <$> phones
  where
    extractPhone :: (Id, Value, Maybe Value, Maybe Value) -> Maybe AndroidPhone
    extractPhone (_, _, Nothing, _) = Nothing
    extractPhone (entry, adb, Just js, ignored) = case fromJSON js of
      Error _ -> Nothing
      Success [] -> Nothing
      Success watched -> do
        adbKey <- fromJSONM adb
        let ignoring = maybe S.empty S.fromList $ ignored >>= fromJSONM
        pure $ AndroidPhone entry adbKey ignoring watched

data AndroidFile = AndroidFile
  { _androidFileEntry :: Id,
    _androidFilePath :: Text
  }
  deriving (Eq, Ord, Show)

makeLenses ''AndroidFile

listFilesForPhone :: (MonadKorrvigs m) => Id -> m (Set AndroidFile)
listFilesForPhone phone = do
  filesSQL <- rSelect $ do
    from <- selectTable entriesMetadataTable
    where_ $ from ^. sqlKey .== sqlStrictText (mtdtSqlName FromAndroid)
    where_ $ from ^. sqlValue .== sqlTextToJson (sqlId phone)
    path <- baseSelectTextMtdt FromAndroidPath $ from ^. sqlEntry
    pure (from ^. sqlEntry, path)
  pure $ S.fromList $ mapMaybe extractFile filesSQL
  where
    extractFile :: (Id, Maybe Text) -> Maybe AndroidFile
    extractFile (_, Nothing) = Nothing
    extractFile (i, Just path) = Just $ AndroidFile i path

importAndroidFiles :: (MonadKorrvigs m) => m (Maybe AndroidPhone)
importAndroidFiles = runMaybeT $ do
  phones <- lift listPhones
  server <- liftIO ADB.startServer
  guard server
  device <- liftIO ADB.connectedDevice >>= hoistMaybe
  liftIO $ putStrLn $ "Found device \"" <> T.unpack device <> "\""
  rt <- lift root
  let importDir = rt </> "captured" </> T.unpack device
  phone <- hoistMaybe $ M.lookup device phones
  liftIO $ putStrLn $ "Recognised device as @" <> T.unpack (unId $ phone ^. androidEntry)
  phoneFiles <- lift $ listFilesForPhone $ phone ^. androidEntry
  forM_ (phone ^. androidWatched) $ \dir -> runMaybeT $ do
    filesOnPhone <- liftIO (ADB.files dir) >>= hoistMaybe
    let filesConsidered = S.difference filesOnPhone (phone ^. androidIgnored)
    let filesToImport = S.difference filesConsidered $ S.map (view androidFilePath) phoneFiles
    let files = S.map T.unpack filesToImport
    liftIO $ putStrLn $ ">>> Importing \"" <> T.unpack dir <> "\""
    let targetDir = importDir <> T.unpack dir
    liftIO $ createDirectoryIfMissing True targetDir
    forM_ files $ \file -> do
      let tgt = targetDir </> file
      ex <- liftIO $ doesFileExist tgt
      let androidPath = T.unpack dir </> file
      if ex
        then liftIO $ putStrLn $ "* " <> file
        else do
          liftIO $ putStrLn file
          success <- liftIO $ ADB.pull androidPath tgt
          unless success $ liftIO $ putStrLn "Failed to import !"
  pure phone
