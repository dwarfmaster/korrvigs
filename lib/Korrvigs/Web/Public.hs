module Korrvigs.Web.Public where

import Data.Text (Text)
import Korrvigs.Web.Backend
import Korrvigs.Web.Collections (getColR)
import Korrvigs.Web.Compute (getEntryComputeR)
import Korrvigs.Web.Download (getEntryDownloadR)
import Korrvigs.Web.Entry (getEntryR)
import Korrvigs.Web.Note (getNoteColR)
import Korrvigs.Web.Public.Crypto
import Korrvigs.Web.Routes
import Korrvigs.Web.Search (getSearchR)
import Yesod hiding (cached, joinPath)

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

getPublicEntryComputeR :: Text -> WebId -> Text -> Handler TypedContent
getPublicEntryComputeR mac i cached = do
  checkMac mac $ EntryComputeR i cached
  getEntryComputeR i cached

getPublicColR :: Text -> [Text] -> Handler Html
getPublicColR mac prefix = do
  checkMac mac $ ColR prefix
  getColR prefix

getPublicSearchR :: Text -> Handler Html
getPublicSearchR mac = do
  checkMac mac SearchR
  getSearchR

getPublicNoteColR :: Text -> WebId -> Text -> Handler TypedContent
getPublicNoteColR mac i col = do
  checkMac mac $ NoteColR i col
  getNoteColR i col
