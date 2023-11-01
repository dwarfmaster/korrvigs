module Korrvigs.Web.Backend where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Web.Routes
import qualified Web.ClientSession as CS
import Yesod

data Korrvigs = Korrvigs Connection

mkYesodData "Korrvigs" korrvigsRoutes

pgsql :: Handler Connection
pgsql = getYesod >>= (\(Korrvigs conn) -> return conn)

jquery :: Widget
jquery =
  addScriptRemoteAttrs
    "https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js"
    [("integrity", "sha384-1H217gwSVyLSIfaLxHbE7dRb3v4mYCKbpQvzx0cegeju1MVsGrX5xXxAvs/HgeFs"), ("crossorigin", "anonymous")]

instance Yesod Korrvigs where
  defaultLayout w = do
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
                $forall (status, msg) <- msgs
                    <p class="message #{status}">#{msg}
                ^{pageBody p}
        |]
    where
      widget = w >> jquery
  jsLoader _ = BottomOfBody
  makeSessionBackend _ = strictSameSiteSessions def
    where
      def = Just <$> defaultClientSessionBackend (24 * 60) CS.defaultKeyFile
