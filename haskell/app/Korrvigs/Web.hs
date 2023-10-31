module Korrvigs.Web where

import Yesod

data Korrvigs = Korrvigs

mkYesod
  "Korrvigs"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod Korrvigs

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello from korrvigs!|]
