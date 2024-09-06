{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Web where

import Korrvigs.Web.Backend
import Korrvigs.Web.Entry
import Korrvigs.Web.Home
import Korrvigs.Web.Login
import Korrvigs.Web.Routes
import Yesod

mkYesodDispatch "WebData" korrvigsRoutes

getSearchR :: Handler Html
getSearchR = logWrap undefined

getEntryDownloadR :: WebId -> Handler TypedContent
getEntryDownloadR (WId _) = logWrap undefined
