{-# LANGUAGE MultiParamTypeClasses #-}

module Korrvigs.Web.Backend where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Korrvigs.Web.Routes
import Text.Cassius (cassiusFile)
import qualified Web.ClientSession as CS
import Yesod

data Korrvigs = Korrvigs
  { korr_connection :: Connection,
    korr_root :: FilePath,
    korr_theme :: Int -> Text
  }

mkYesodData "Korrvigs" korrvigsRoutes

pgsql :: Handler Connection
pgsql = getYesod >>= return . korr_connection

jquery :: Widget
jquery =
  addScriptRemoteAttrs
    "https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js"
    [("integrity", "sha384-1H217gwSVyLSIfaLxHbE7dRb3v4mYCKbpQvzx0cegeju1MVsGrX5xXxAvs/HgeFs"), ("crossorigin", "anonymous")]

getBase :: Handler (Int -> Text)
getBase = getYesod >>= return . korr_theme

korrRoot :: Handler FilePath
korrRoot = getYesod >>= return . korr_root

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

instance RenderMessage Korrvigs FormMessage where
  renderMessage _ _ = defaultFormMessage
