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
/ HomeR GET

-- Entry search and visualisation
/entry SearchR GET
/entry/#WebId EntryR GET
/entry/#WebId/download EntryDownloadR GET
/entry/#WebId/metadata EntryMtdtR GET POST
/entry/#WebId/cache EntryCacheR GET
/entry/#WebId/cache/#Text EntryComputeR GET

-- Note getting
/note/#WebId/sub NoteR GET POST
/note/#WebId/sub/#WebAnyLoc NoteSubR GET POST
/note/#WebId/col/#Text NoteColR GET POST

-- Collections
/collections/*[Text] ColR GET

-- Actions
/action/#Text/entry/#WebId ActEntryR POST
/action/#Text/home ActHomeR POST
/action/#Text/collection/*[Text] ActColR POST
/action/#Text/search ActSearchR POST

-- Git visualisation and manipulation
/git GitR GET POST

-- Data
/static StaticR Static getStaticR
/favicon.ico FaviconR GET

-- Public
/public PublicR GET
/public/#Text/entry/#WebId PublicEntryR GET
/public/#Text/entry/#WebId/download PublicEntryDownloadR GET
/public/#Text/entry/#WebId/cache/#Text PublicEntryComputeR GET
/public/#Text/collections/*[Text] PublicColR GET
/public/#Text/entry PublicSearchR GET
/public/#Text/note/#WebId/col/#Text PublicNoteColR GET

-- Specific visualisations
-- TODO Date
-- TODO Place
-- TODO Graph
|]
