module Korrvigs.Web.Routes (korrvigsRoutes, UUID) where

import Korrvigs.Web.UUID (UUID)
import Yesod (parseRoutes)
import Yesod.Routes.TH.Types (ResourceTree)

korrvigsRoutes :: [ResourceTree String]
korrvigsRoutes =
  [parseRoutes|
/ HomeR GET
/entry AllEntriesR GET

/entry/#UUID EntryR GET POST
/entry/#UUID/edit EntryEditR GET POST
/entry/#UUID/query/#Text EntryQueryR GET

/entry/#UUID/sub/#Text EntrySubR GET
/entry/#UUID/sub/#Text/content EntrySubContentR GET
/entry/#UUID/sub/#Text/query/#Text EntrySubQueryR GET

/generate/classes GenerateClassesR GET
/generate/rels/sql GenerateRelsSqlR GET
/generate/rels/hs GenerateRelsHsR GET
|]
