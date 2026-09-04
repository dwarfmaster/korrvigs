{-# LANGUAGE DataKinds #-}

module Korrvigs.Web.Public.Crypto where

import Control.Lens
import Control.Monad
import Crypto.Hash.Algorithms
import Crypto.MAC.KeyedBlake2
import Data.Base64.Types
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time
import Data.Time.Format.ISO8601
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

renderRouteForSigning :: (Route WebData -> [(Text, Text)] -> Text) -> Route WebData -> [(Text, Text)] -> Maybe Day -> ByteString
renderRouteForSigning render route params deadline =
  Enc.encodeUtf8 $ deadlineStr <> "//" <> render route params
  where
    deadlineStr = case deadline of
      Nothing -> "always"
      Just d -> T.pack $ iso8601Show d

mkRouteSigner ::
  ByteString ->
  (Route WebData -> [(Text, Text)] -> Text) ->
  Route WebData ->
  [(Text, Text)] ->
  Maybe Day ->
  Text
mkRouteSigner secret render route params deadline =
  extractBase64 . B64.encodeBase64 . BS.pack . BA.unpack . keyedBlake2GetDigest $ cmac
  where
    url = renderRouteForSigning render route params deadline
    cmac :: KeyedBlake2 Algo = keyedBlake2 secret url

signRoute :: Route WebData -> [(Text, Text)] -> Maybe Day -> Handler Text
signRoute route params deadline =
  getsYesod web_route_signer <*> getUrlRenderParams <*> pure route <*> pure params <*> pure deadline

checkMac :: Route WebData -> SubHandlerFor PublicSubSite WebData ()
checkMac route = do
  sub <- getSubYesod
  let mac64 = sub ^. publicHash
  let deadline = sub ^. publicDeadline
  secret <- getsYesod web_mac_secret
  render <- getUrlRenderParams
  params <- reqGetParams <$> getRequest
  let troute = renderRouteForSigning render route params deadline
  let cmac :: KeyedBlake2 Algo = keyedBlake2 secret troute
  let cmacBS = BS.pack . BA.unpack . keyedBlake2GetDigest $ cmac
  case (== cmacBS) <$> B64.decodeBase64Untyped (Enc.encodeUtf8 mac64) of
    Left _ -> notFound
    Right False -> permissionDenied "Invalid MAC"
    Right True -> do
      forM_ deadline $ \dday -> do
        currentDay <- liftIO $ utctDay <$> getCurrentTime
        when (currentDay > dday) $ permissionDenied "Expired link"
      pure ()

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

mkPublicAlways :: Route WebData -> [(Text, Text)] -> Maybe Day -> Handler (Route WebData)
mkPublicAlways r attrs deadline = case mkPublicRoute r of
  Nothing -> pure PublicR
  Just publicR -> do
    mac <- signRoute r attrs deadline
    pure $ case deadline of
      Nothing -> PublicSubR mac publicR
      Just d -> PublicSubDayR mac d publicR

mkPublic :: Route WebData -> Handler (Route WebData)
mkPublic r =
  getCurrentRoute >>= \case
    Just PublicR -> mkPublicAlways r [] Nothing
    Just (PublicSubR _ _) -> mkPublicAlways r [] Nothing
    Nothing -> mkPublicAlways r [] Nothing
    Just (PublicSubDayR _ deadline _) -> mkPublicAlways r [] $ Just deadline
    _ -> pure r
