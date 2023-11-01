module Korrvigs.Web.Routes (korrvigsRoutes, UUID) where

import Korrvigs.Web.UUID (UUID)
import Yesod (parseRoutes)
import Yesod.Routes.TH.Types (ResourceTree)

korrvigsRoutes :: [ResourceTree String]
korrvigsRoutes =
  [parseRoutes|
/ HomeR GET
/entry AllEntriesR GET
/entry/#UUID EntryR GET
/entry/#UUID/#Text EntryQueryR GET
/entity/#UUID/#Text EntitySubR GET
/entity/#UUID/#Text/#Text EntitySubQueryR GET
|]
