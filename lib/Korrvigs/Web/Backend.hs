module Korrvigs.Web.Backend where

import Data.Binary.Builder
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Monad
import Korrvigs.Utils (lazyCreateManager)
import Korrvigs.Utils.Base16
import Korrvigs.Utils.JSON
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Network.HTTP.Client hiding (path)
import Network.HTTP.Types
import Yesod
import Yesod.Static

data WebData = WebData
  { web_connection :: Connection,
    web_root :: FilePath,
    web_theme :: Base16Index -> Text,
    web_static :: Static,
    web_static_redirect :: Maybe Text,
    web_mac_secret :: ByteString,
    web_calsync_root :: FilePath,
    web_capture_root :: FilePath,
    web_credentials :: Map Text Value,
    web_manager :: IORef (Maybe Manager)
  }

getStaticR :: WebData -> Static
getStaticR = web_static

mkYesodData "WebData" korrvigsRoutes

getBase :: Handler (Base16Index -> Text)
getBase = web_theme <$> getYesod

hdIsEntry :: Route WebData -> Bool
hdIsEntry SearchR = True
hdIsEntry (EntryR _) = True
hdIsEntry (EntryDownloadR _) = True
hdIsEntry (EntryMtdtR _) = True
hdIsEntry (NoteR _) = True
hdIsEntry (NoteSubR _ _) = True
hdIsEntry (NoteColR _ _) = True
hdIsEntry _ = False

headerContent :: [(Text, Route WebData, Route WebData -> Bool)]
headerContent =
  [ ("Home", HomeR, (== HomeR)),
    ("Entry", SearchR, hdIsEntry),
    ("Git", GitR, (== GitR))
  ]

mkHeader :: Handler Widget
mkHeader =
  isPublic >>= \case
    True -> pure mempty
    False ->
      getCurrentRoute
        <&> Rcs.header . \case
          Just route -> [(current route, txt, rt) | (txt, rt, current) <- headerContent]
          Nothing -> [(False, txt, rt) | (txt, rt, _) <- headerContent]

mkQuery :: (Text, Text) -> QueryItem
mkQuery (key, val) | T.null val = (Enc.encodeUtf8 key, Nothing)
mkQuery (key, val) = (Enc.encodeUtf8 key, Just $ Enc.encodeUtf8 val)

isPublicRoute :: Route WebData -> Bool
isPublicRoute PublicR = True
isPublicRoute (PublicEntryR _ _) = True
isPublicRoute (PublicEntryDownloadR _ _) = True
isPublicRoute (PublicEntryComputeR {}) = True
isPublicRoute (PublicSearchR _) = True
isPublicRoute (PublicNoteColR {}) = True
isPublicRoute _ = False

isPublic :: Handler Bool
isPublic = maybe True isPublicRoute <$> getCurrentRoute

instance Yesod WebData where
  jsLoader _ = BottomOfBody
  makeSessionBackend _ = pure Nothing
  maximumContentLength _ _ = Nothing
  defaultLayout w = do
    base <- getBase
    hd <- mkHeader
    let widget = sequence_ [Rcs.defaultCss base StaticR, hd, w]
    p <- widgetToPageContent widget
    msgs <- getMessages
    withUrlRenderer
      [hamlet|
        $newline never
        $doctype 5
        <html>
          <head>
            <title>#{pageTitle p}
            $maybe description <- pageDescription p
              <meta name="description" content="#{description}">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            ^{pageHead p}
          <body>
            <div #central>
              $forall (status,msg) <- msgs
                <p class="message #{status}">#{msg}
              ^{pageBody p}
        |]
  urlParamRenderOverride web (StaticR (StaticRoute route _)) query =
    case web_static_redirect web of
      Nothing -> Nothing
      Just url ->
        let httpQuery = mkQuery <$> query
         in let path = encodePath route httpQuery
             in Just $ fromByteString (Enc.encodeUtf8 url) <> path
  urlParamRenderOverride _ _ _ = Nothing

instance RenderMessage WebData FormMessage where
  renderMessage _ _ = defaultFormMessage

instance MonadKorrvigs Handler where
  pgSQL = getsYesod web_connection
  root = getsYesod web_root
  calsyncRoot = getsYesod web_calsync_root
  captureRoot = getsYesod web_capture_root
  getCredential c = do
    creds <- getsYesod web_credentials
    pure $ M.lookup c creds >>= fromJSONM
  manager = getsYesod web_manager >>= liftIO . lazyCreateManager

getFaviconR :: Handler TypedContent
getFaviconR = redirect $ StaticR $ StaticRoute ["favicon.ico"] []
