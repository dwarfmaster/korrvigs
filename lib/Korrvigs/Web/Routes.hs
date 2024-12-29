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
/ HomeR GET
/login LoginR GET POST

-- Entry search and visualisation
/entry SearchR GET
/entry/#WebId EntryR GET POST
/entry/#WebId/download EntryDownloadR GET
/entry/#WebId/metadata EntryMtdtR GET POST

-- Git visualisation and manipulation
/git GitR GET POST

-- Data
/static StaticR Static getStaticR

-- Specific visualisations
-- TODO Date
-- TODO Place
-- TODO Graph
|]
