{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Web where

import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

mkYesodDispatch "WebData" korrvigsRoutes

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello from korrvigs!|]

postHomeR :: Handler Html
postHomeR = undefined

getSearchR :: Handler Html
getSearchR = undefined

getEntryR :: WebId -> Handler Html
getEntryR (WId _) = undefined

postEntryR :: WebId -> Handler Html
postEntryR (WId _) = undefined

getEntryDownloadR :: WebId -> Handler TypedContent
getEntryDownloadR (WId _) = undefined
