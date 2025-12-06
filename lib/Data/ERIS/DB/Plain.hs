module Data.ERIS.DB.Plain where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as B32
import Data.ERIS.Crypto
import Data.ERIS.DB.Class
import qualified Data.Text as T
import System.Directory
import System.FilePath

newtype ERISFileDB = ERISFileDB {_dbRoot :: FilePath}

makeLenses ''ERISFileDB

dbPath :: FilePath -> ERISHash -> FilePath
dbPath root hsh = joinPath [root, prefix, T.unpack b32]
  where
    b32 = B32.encodeBase32Unpadded $ BS.pack $ BA.unpack hsh
    prefix = T.unpack $ T.take 2 b32

instance (MonadIO m) => ERISBlockRead ERISFileDB m where
  erisBlockStorageGet db hsh =
    liftIO $
      doesFileExist path >>= \ex ->
        if ex
          then Just <$> BS.readFile path
          else pure Nothing
    where
      path = dbPath (db ^. dbRoot) hsh

instance (MonadIO m) => ERISBlockWrite ERISFileDB m where
  erisBlockStoragePut db hsh =
    liftIO . BS.writeFile (dbPath (db ^. dbRoot) hsh)
