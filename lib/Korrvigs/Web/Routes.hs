module Korrvigs.Web.Routes where

import Control.Lens
import Korrvigs.Entry (Id (..))
import Korrvigs.Note.Loc (AnyLoc, parseLoc, renderLoc)
import Yesod
import Yesod.Routes.TH.Types (ResourceTree)

newtype WebId = WId Id
  deriving (Eq, Show, Read)

instance PathPiece WebId where
  toPathPiece (WId i) = unId i
  fromPathPiece s = Just $ WId $ MkId s

newtype WebAnyLoc = WLoc AnyLoc
  deriving (Eq, Show, Read)

instance PathPiece WebAnyLoc where
  toPathPiece (WLoc loc) = renderLoc loc
  fromPathPiece s = parseLoc s ^? _Right . to WLoc

korrvigsRoutes :: [ResourceTree String]
korrvigsRoutes =
  [parseRoutes|
-- Home and login
/ HomeR GET POST

-- Entry search and visualisation
/entry SearchR GET
/entry/#WebId EntryR GET POST
/entry/#WebId/download EntryDownloadR GET
/entry/#WebId/metadata EntryMtdtR GET POST
/entry/#WebId/parents EntryParentsR GET POST
/entry/#WebId/cache EntryCacheR GET
/entry/#WebId/cache/#Text EntryComputeR GET

-- Note getting
/note/#WebId/sub NoteR GET POST
/note/#WebId/sub/#WebAnyLoc NoteSubR GET POST

-- Collections
/collections ColR GET
/collections/favourite/*[Text] ColFavouriteR GET
/collections/misc/*[Text] ColMiscR GET
/collections/gallery/*[Text] ColGalR GET

-- Git visualisation and manipulation
/git GitR GET POST

-- Data
/static StaticR Static getStaticR
/favicon.ico FaviconR GET

-- Specific visualisations
-- TODO Date
-- TODO Place
-- TODO Graph
|]
