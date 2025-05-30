{-# LANGUAGE DataKinds #-}

module Korrvigs.Web.Public.Crypto where

import Crypto.Hash.Algorithms
import Crypto.MAC.KeyedBlake2
import Data.Base64.Types
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import Korrvigs.Web.Backend
import System.Directory
import System.Entropy
import System.Environment
import System.FilePath
import System.Posix.Files
import Yesod hiding (cached, joinPath)

type Algo = Blake2b 160

loadOrGenerateKey :: IO ByteString
loadOrGenerateKey = do
  xdg <- lookupEnv "XDG_DATA_HOME"
  baseDir <- case xdg of
    Just pth -> pure pth
    Nothing -> joinPath . (: [".local", "share"]) <$> getEnv "HOME"
  let dir = joinPath [baseDir, "korrvigs"]
  let file = joinPath [dir, "mac_secret"]
  ex <- doesFileExist file
  if ex
    then BS.readFile file
    else do
      createDirectoryIfMissing True dir
      BS.writeFile file ""
      setFileMode file (unionFileModes ownerReadMode ownerWriteMode)
      key <- getEntropy 512
      BS.writeFile file key
      pure key

signRoute :: Route WebData -> [(Text, Text)] -> Handler Text
signRoute route params = do
  render <- getUrlRenderParams
  let url = render route params
  secret <- getsYesod web_mac_secret
  let cmac :: KeyedBlake2 Algo = keyedBlake2 secret $ Enc.encodeUtf8 url
  pure . extractBase64 . B64.encodeBase64 . BS.pack . BA.unpack . keyedBlake2GetDigest $ cmac

checkMac :: Text -> Route WebData -> Handler ()
checkMac mac64 route = do
  secret <- getsYesod web_mac_secret
  render <- getUrlRenderParams
  params <- reqGetParams <$> getRequest
  let troute = Enc.encodeUtf8 $ render route params
  let cmac :: KeyedBlake2 Algo = keyedBlake2 secret troute
  let cmacBS = BS.pack . BA.unpack . keyedBlake2GetDigest $ cmac
  case (== cmacBS) <$> B64.decodeBase64Untyped (Enc.encodeUtf8 mac64) of
    Left _ -> notFound
    Right False -> permissionDenied "Invalid MAC"
    Right True -> pure ()

mkPublicImpl :: Route WebData -> Handler (Route WebData)
mkPublicImpl r@(EntryR i) = PublicEntryR <$> signRoute r [] <*> pure i
mkPublicImpl r@(EntryDownloadR i) = PublicEntryDownloadR <$> signRoute r [] <*> pure i
mkPublicImpl r@(EntryComputeR i cached) = PublicEntryComputeR <$> signRoute r [] <*> pure i <*> pure cached
mkPublicImpl _ = pure PublicR

mkPublic :: Route WebData -> Handler (Route WebData)
mkPublic r =
  isPublic >>= \case
    True -> mkPublicImpl r
    False -> pure r
