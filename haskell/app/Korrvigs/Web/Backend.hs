module Korrvigs.Web.Backend where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Web.Routes
import Text.Cassius (cassiusFile)
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

getBase :: Handler (Int -> Text)
getBase = pure base
  where
    base :: Int -> Text
    base 0 = "#231e18"
    base 1 = "#302b25"
    base 2 = "#48413a"
    base 3 = "#9d8b70"
    base 4 = "#b4a490"
    base 5 = "#cabcb1"
    base 6 = "#d7c8bc"
    base 7 = "#e4d4c8"
    base 8 = "#d35c5c"
    base 9 = "#ca7f32"
    base 10 = "#e0ac16"
    base 11 = "#b7ba53"
    base 12 = "#6eb958"
    base 13 = "#88a4d3"
    base 14 = "#bb90e2"
    base 15 = "b49368"
    base _ = error "Unsupported base16 color"

css :: (Int -> Text) -> Widget
css base = toWidget $(cassiusFile "app/Korrvigs/Web/Ressources/css/default.cassius")

instance Yesod Korrvigs where
  defaultLayout w = do
    base <- getBase
    let widget = sequence_ [css base, jquery, w]
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
                    $forall (status, msg) <- msgs
                        <p class="message #{status}">#{msg}
                    ^{pageBody p}
        |]
  jsLoader _ = BottomOfBody
  makeSessionBackend _ = strictSameSiteSessions def
    where
      def = Just <$> defaultClientSessionBackend (24 * 60) CS.defaultKeyFile
