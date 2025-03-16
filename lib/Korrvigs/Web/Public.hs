module Korrvigs.Web.Public where

import Data.Text (Text)
import Korrvigs.Web.Backend
import Korrvigs.Web.Collections (getColGalR, getColMiscR, getColTaskR)
import Korrvigs.Web.Download (getEntryDownloadR)
import Korrvigs.Web.Entry (getEntryR)
import Korrvigs.Web.Public.Crypto
import Korrvigs.Web.Routes
import Yesod hiding (joinPath)

getPublicR :: Handler Html
getPublicR = do
  defaultLayout
    [whamlet|
  <h1> Forbidden
  <p>You have tried to access a private part of the website.
  |]

getPublicEntryR :: Text -> WebId -> Handler Html
getPublicEntryR mac i = do
  checkMac mac $ EntryR i
  getEntryR i

getPublicEntryDownloadR :: Text -> WebId -> Handler TypedContent
getPublicEntryDownloadR mac i = do
  checkMac mac $ EntryDownloadR i
  getEntryDownloadR i

getPublicColMiscR :: Text -> [Text] -> Handler Html
getPublicColMiscR mac prefix = do
  checkMac mac $ ColMiscR prefix
  getColMiscR prefix

getPublicColGalR :: Text -> [Text] -> Handler Html
getPublicColGalR mac prefix = do
  checkMac mac $ ColGalR prefix
  getColGalR prefix

getPublicColTaskR :: Text -> [Text] -> Handler Html
getPublicColTaskR mac prefix = do
  checkMac mac $ ColTaskR prefix
  getColTaskR prefix
