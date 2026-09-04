module Korrvigs.Web.Backend where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens
import Data.Binary.Builder
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Monad
import Korrvigs.Utils (lazyCreateManager)
import Korrvigs.Utils.Base16
import Korrvigs.Utils.JSON
import Korrvigs.Web.Ressources (CssFile)
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Network.HTTP.Client hiding (path)
import Network.HTTP.Types
import Text.Cassius (Css)
import Yesod
import Yesod.Static

data WebData = WebData
  { web_connection :: Connection,
    web_sql_lock :: MVar (),
    web_root :: FilePath,
    web_theme :: Base16Index -> Text,
    web_static :: Static,
    web_static_redirect :: Maybe Text,
    web_mime_database :: FilePath,
    web_mac_secret :: ByteString,
    web_route_signer :: (Route WebData -> [(Text, Text)] -> Text) -> Route WebData -> [(Text, Text)] -> Maybe Day -> Text,
    web_capture_root :: FilePath,
    web_credentials :: Map Text Value,
    web_manager :: IORef (Maybe Manager),
    web_log_context :: IORef (Maybe Int),
    web_tokens :: TVar (Map Text Value)
  }

getStaticR :: WebData -> Static
getStaticR = web_static

data PublicSubSite = PublicSubSite
  { _publicHash :: Text,
    _publicDeadline :: Maybe Day
  }

mkYesodSubData "PublicSubSite" publicRoutes
makeLenses ''PublicSubSite

getPublicSubSite :: WebData -> Text -> PublicSubSite
getPublicSubSite _ hsh = PublicSubSite hsh Nothing

getPublicSubSiteDay :: WebData -> Text -> Day -> PublicSubSite
getPublicSubSiteDay _ hsh day = PublicSubSite hsh $ Just day

mkYesodData "WebData" korrvigsRoutes

getBase :: Handler (Base16Index -> Text)
getBase = web_theme <$> getYesod

hdIsHome :: Route WebData -> Bool
hdIsHome HomeR = True
hdIsHome (DateByDayR _) = True
hdIsHome (DateByWeekR _ _) = True
hdIsHome _ = False

hdIsEntry :: Route WebData -> Bool
hdIsEntry SearchR = True
hdIsEntry (EntryR _) = True
hdIsEntry (EntryDownloadR _) = True
hdIsEntry (EntryMtdtR _) = True
hdIsEntry _ = False

hdIsNote :: Route WebData -> Bool
hdIsNote NoteFuzzyR = True
hdIsNote (NoteR _) = True
hdIsNote (NoteSubR _ _) = True
hdIsNote (NoteColR _ _) = True
hdIsNote (NoteColEditR _ _) = True
hdIsNote (NoteSyndicateR _) = True
hdIsNote (NoteSyndicateSingleR _ _) = True
hdIsNote _ = False

headerContent :: [(Text, Route WebData, Route WebData -> Bool)]
headerContent =
  [ ("Home", HomeR, hdIsHome),
    ("Entry", SearchR, hdIsEntry),
    ("Note", NoteFuzzyR, hdIsNote)
  ]

mkCss :: Handler (CssFile -> Route WebData)
mkCss =
  isPublic >>= \case
    True -> do
      render <- getUrlRenderParams
      signer <- getsYesod web_route_signer
      pure $ \css -> PublicSubR (signer render (CssR css) [] Nothing) $ PublicCssR css
    False -> pure CssR

mkHeader :: Handler Widget
mkHeader =
  isPublic >>= \case
    True -> pure mempty
    False -> do
      cssR <- mkCss
      getCurrentRoute
        <&> Rcs.header StaticR cssR . \case
          Just route -> [(current route, txt, rt) | (txt, rt, current) <- headerContent]
          Nothing -> [(False, txt, rt) | (txt, rt, _) <- headerContent]

mkQuery :: (Text, Text) -> QueryItem
mkQuery (key, val) | T.null val = (Enc.encodeUtf8 key, Nothing)
mkQuery (key, val) = (Enc.encodeUtf8 key, Just $ Enc.encodeUtf8 val)

isPublicRoute :: Route WebData -> Bool
isPublicRoute PublicR = True
isPublicRoute (PublicSubR _ _) = True
isPublicRoute (PublicSubDayR _ _ _) = True
isPublicRoute _ = False

isPublic :: Handler Bool
isPublic = maybe True isPublicRoute <$> getCurrentRoute

instance Yesod WebData where
  jsLoader _ = BottomOfBody
  makeSessionBackend _ = pure Nothing
  maximumContentLength _ _ = Nothing
  defaultLayout w = do
    hd <- mkHeader
    cssR <- mkCss
    let widget = sequence_ [Rcs.defaultCss cssR, hd, w]
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
  lockSQL = do
    lock <- getsYesod web_sql_lock
    liftIO $ takeMVar lock
    getsYesod web_connection
  unlockSQL = do
    lock <- getsYesod web_sql_lock
    liftIO $ putMVar lock ()
  root = getsYesod web_root
  captureRoot = getsYesod web_capture_root
  mimeDatabase = getsYesod web_mime_database
  getCredential c = do
    creds <- getsYesod web_credentials
    pure $ M.lookup c creds >>= fromJSONM
  manager = getsYesod web_manager >>= liftIO . lazyCreateManager
  getToken tok = do
    tv <- getsYesod web_tokens
    liftIO $ atomically $ do
      toks <- readTVar tv
      pure $ M.lookup tok toks >>= fromJSONM
  storeToken tok v = do
    tv <- getsYesod web_tokens
    liftIO $ atomically $ modifyTVar tv $ M.insert tok $ toJSON v
  registerLogContext ctx = do
    ctxRef <- getsYesod web_log_context
    liftIO $ writeIORef ctxRef ctx
  getLogContext = do
    ctxRef <- getsYesod web_log_context
    liftIO $ readIORef ctxRef

instance MonadKorrvigs (SubHandlerFor PublicSubSite WebData) where
  lockSQL = liftHandler lockSQL
  unlockSQL = liftHandler unlockSQL
  root = liftHandler root
  captureRoot = liftHandler captureRoot
  mimeDatabase = liftHandler mimeDatabase
  getCredential = liftHandler . getCredential
  manager = liftHandler manager
  getToken = liftHandler . getToken
  storeToken tok = liftHandler . storeToken tok
  registerLogContext = liftHandler . registerLogContext
  getLogContext = liftHandler getLogContext

getFaviconR :: Handler TypedContent
getFaviconR = redirect $ StaticR $ StaticRoute ["favicon.ico"] []

getCssR :: CssFile -> Handler Css
getCssR css = do
  base <- getBase
  render <- getUrlRenderParams
  pure $ Rcs.resolveCSS base StaticR css render
