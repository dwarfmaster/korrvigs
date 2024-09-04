module Korrvigs.Web.Home where

import Korrvigs.Web.Backend
import Korrvigs.Web.Login (logWrap)
import Yesod

getHomeR :: Handler Html
getHomeR = logWrap $ defaultLayout [whamlet|Hello from korrvigs!|]

postHomeR :: Handler Html
postHomeR = logWrap undefined
