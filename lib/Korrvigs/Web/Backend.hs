module Korrvigs.Web.Backend where

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

instance Yesod WebData where
  jsLoader _ = BottomOfBody
  makeSessionBackend _ =
    strictSameSiteSessions $
      Just <$> defaultClientSessionBackend (24 * 60) CS.defaultKeyFile
  defaultLayout w = do
    base <- getBase
    let widget = sequence_ [Rcs.defaultCss base, w]
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
