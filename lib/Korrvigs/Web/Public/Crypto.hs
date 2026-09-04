{-# LANGUAGE DataKinds #-}

module Korrvigs.Web.Public.Crypto where

import Control.Lens
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

mkRouteSigner ::
  ByteString ->
  (Route WebData -> [(Text, Text)] -> Text) ->
  Route WebData ->
  [(Text, Text)] ->
  Text
mkRouteSigner secret render route params =
  extractBase64 . B64.encodeBase64 . BS.pack . BA.unpack . keyedBlake2GetDigest $ cmac
  where
    url = render route params
    cmac :: KeyedBlake2 Algo = keyedBlake2 secret $ Enc.encodeUtf8 url

signRoute :: Route WebData -> [(Text, Text)] -> Handler Text
signRoute route params =
  getsYesod web_route_signer <*> getUrlRenderParams <*> pure route <*> pure params

checkMac :: Route WebData -> SubHandlerFor PublicSubSite WebData ()
checkMac route = do
  sub <- getSubYesod
  let mac64 = sub ^. publicHash
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

mkPublicRoute :: Route WebData -> Maybe (Route PublicSubSite)
mkPublicRoute (EntryR i) = Just $ PublicEntryR i
mkPublicRoute (EntryDownloadR i) = Just $ PublicEntryDownloadR i
mkPublicRoute (EntryComputeR i cached) = Just $ PublicEntryComputeR i cached
mkPublicRoute (CssR css) = Just $ PublicCssR css
mkPublicRoute SearchR = Just PublicSearchR
mkPublicRoute (NoteColR i col) = Just $ PublicNoteColR i col
mkPublicRoute (NoteNamedSubR i sub) = Just $ PublicNoteNamedSubR i sub
mkPublicRoute (NoteNamedCodeR i code) = Just $ PublicNoteNamedCodeR i code
mkPublicRoute (BlogTopR blog) = Just $ PublicBlogTopR blog
mkPublicRoute (BlogPostR post) = Just $ PublicBlogPostR post
mkPublicRoute _ = Nothing

mkPublicAlways :: Route WebData -> [(Text, Text)] -> Handler (Route WebData)
mkPublicAlways r attrs = case mkPublicRoute r of
  Nothing -> pure PublicR
  Just publicR -> PublicSubR <$> signRoute r attrs <*> pure publicR

mkPublic :: Route WebData -> Handler (Route WebData)
mkPublic r =
  isPublic >>= \case
    True -> mkPublicAlways r []
    False -> pure r
