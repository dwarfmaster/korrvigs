module Korrvigs.Web.Public where

import Data.Text (Text)
import Korrvigs.Web.Backend
import Korrvigs.Web.Blog
import Korrvigs.Web.Compute (getEntryComputeR)
import Korrvigs.Web.Download (getEntryDownloadR)
import Korrvigs.Web.Entry (getEntryR)
import Korrvigs.Web.Note (getNoteColR, getNoteNamedCodeR, getNoteNamedSubR)
import Korrvigs.Web.Public.Crypto
import Korrvigs.Web.Ressources
import Korrvigs.Web.Routes
import Korrvigs.Web.Search (getSearchR)
import Text.Cassius (Css)
import Yesod hiding (cached, joinPath)

type PublicHandler = SubHandlerFor PublicSubSite WebData

getPublicR :: Handler Html
getPublicR = do
  defaultLayout
    [whamlet|
  <h1> Forbidden
  <p>You have tried to access a private part of the website.
  |]

getPublicCssR :: CssFile -> PublicHandler Css
getPublicCssR css = do
  checkMac $ CssR css
  liftHandler $ getCssR css

getPublicEntryR :: WebId -> PublicHandler Html
getPublicEntryR i = do
  checkMac $ EntryR i
  liftHandler $ getEntryR i

getPublicEntryDownloadR :: WebId -> PublicHandler TypedContent
getPublicEntryDownloadR i = do
  checkMac $ EntryDownloadR i
  liftHandler $ getEntryDownloadR i

getPublicEntryComputeR :: WebId -> Text -> PublicHandler TypedContent
getPublicEntryComputeR i cached = do
  checkMac $ EntryComputeR i cached
  liftHandler $ getEntryComputeR i cached

getPublicSearchR :: PublicHandler Html
getPublicSearchR = do
  checkMac SearchR
  liftHandler getSearchR

getPublicNoteColR :: WebId -> Text -> PublicHandler TypedContent
getPublicNoteColR i col = do
  checkMac $ NoteColR i col
  liftHandler $ getNoteColR i col

getPublicNoteNamedSubR :: WebId -> Text -> PublicHandler Html
getPublicNoteNamedSubR i sb = do
  checkMac $ NoteNamedSubR i sb
  liftHandler $ getNoteNamedSubR i sb

getPublicNoteNamedCodeR :: WebId -> Text -> PublicHandler Html
getPublicNoteNamedCodeR i cd = do
  checkMac $ NoteNamedCodeR i cd
  liftHandler $ getNoteNamedCodeR i cd

getPublicBlogTopR :: Text -> PublicHandler TypedContent
getPublicBlogTopR top = do
  checkMac $ BlogTopR top
  liftHandler $ getBlogTopR top

getPublicBlogPostR :: Text -> PublicHandler TypedContent
getPublicBlogPostR post = do
  checkMac $ BlogPostR post
  liftHandler $ getBlogPostR post
