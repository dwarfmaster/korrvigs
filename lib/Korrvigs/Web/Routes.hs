module Korrvigs.Web.Routes where

import Korrvigs.Entry (Id (..))
import Yesod
import Yesod.Routes.TH.Types (ResourceTree)

newtype WebId = WId Id
  deriving (Eq, Show, Read)

instance PathPiece WebId where
  toPathPiece (WId i) = unId i
  fromPathPiece s = Just $ WId $ MkId s

korrvigsRoutes :: [ResourceTree String]
korrvigsRoutes =
  [parseRoutes|
-- Home and login
/ HomeR GET POST
/login LoginR GET POST

-- Entry search and visualisation
/entry SearchR GET
/entry/#WebId EntryR GET POST
/entry/#WebId/download EntryDownloadR GET
|]
