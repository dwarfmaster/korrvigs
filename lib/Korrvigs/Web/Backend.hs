module Korrvigs.Web.Backend where

import Data.Functor ((<&>))
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import qualified Korrvigs.Actions as Actions
import Korrvigs.Monad
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import qualified Web.ClientSession as CS
import Yesod

data WebData = WebData
  { web_connection :: Connection,
    web_root :: FilePath,
    web_theme :: Int -> Text -- Base16 theme
  }

mkYesodData "WebData" korrvigsRoutes

getBase :: Handler (Int -> Text)
getBase = web_theme <$> getYesod

hdIsEntry :: Route WebData -> Bool
hdIsEntry SearchR = True
hdIsEntry (EntryR _) = True
hdIsEntry (EntryDownloadR _) = True
hdIsEntry _ = False

headerContent :: [(Text, Route WebData, Route WebData -> Bool)]
headerContent =
  [ ("Home", HomeR, (== HomeR)),
    ("Entry", SearchR, hdIsEntry)
  ]

mkHeader :: Handler Widget
mkHeader =
  getCurrentRoute
    <&> Rcs.header . \case
      Just route -> [(current route, txt, rt) | (txt, rt, current) <- headerContent]
      Nothing -> [(False, txt, rt) | (txt, rt, _) <- headerContent]

instance Yesod WebData where
  jsLoader _ = BottomOfBody
  makeSessionBackend _ =
    strictSameSiteSessions $
      Just <$> defaultClientSessionBackend (24 * 60) CS.defaultKeyFile
  defaultLayout w = do
    base <- getBase
    hd <- mkHeader
    let widget = sequence_ [Rcs.defaultCss base, hd, w]
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
            ^{pageHead p}
          <body>
            <div #central>
              $forall (status,msg) <- msgs
                <p class="message #{status}">#{msg}
              ^{pageBody p}
        |]

instance RenderMessage WebData FormMessage where
  renderMessage _ _ = defaultFormMessage

instance MonadKorrvigs Handler where
  pgSQL = getsYesod web_connection
  root = getsYesod web_root
  load = Actions.load
  remove = Actions.remove
  dispatchRemove = Actions.dispatchRemove
  removeDB = Actions.removeDB
  dispatchRemoveDB = Actions.dispatchRemoveDB
  sync = Actions.sync
