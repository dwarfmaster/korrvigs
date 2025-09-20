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
/bibtex SearchBibtexR GET

-- Entry search and visualisation
/entry SearchR GET
/entry/#WebId EntryR GET
/entry/#WebId/download EntryDownloadR GET
/entry/#WebId/metadata EntryMtdtR GET POST
/entry/#WebId/cache EntryCacheR GET
/entry/#WebId/cache/#Text EntryComputeR GET
/entry/#WebId/bibtex EntryBibtexR GET

-- Note getting
/note NoteFuzzyR GET
/note/#WebId/sub NoteR GET POST
/note/#WebId/sub/#WebAnyLoc NoteSubR GET POST
/note/#WebId/col/#Text NoteColR GET POST
/note/#WebId/col/#Text/bibtex NoteColBibtexR GET
/note/#WebId/col/#Text/edit NoteColEditR GET POST
/note/#WebId/named/sub/#Text NoteNamedSubR GET
/note/#WebId/named/code/#Text NoteNamedCodeR GET

-- Actions
/action/#Text/entry/#WebId ActEntryR POST
/action/#Text/home ActHomeR POST
/action/#Text/search ActSearchR POST
/action/#Text/note/#WebId/col/#Text ActNoteColR POST
/action/#Text/note/#WebId/sub/#Text ActNoteSubR POST
/action/#Text/note/#WebId/code/#Text ActNoteCodeR POST

-- Misc
/syndicate/#WebId/#Int SynItemR GET

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
/public/#Text/entry PublicSearchR GET
/public/#Text/note/#WebId/col/#Text PublicNoteColR GET
/public/#Text/note/#WebId/named/sub/#Text PublicNoteNamedSubR GET
/public/#Text/note/#WebId/named/code/#Text PublicNoteNamedCodeR GET

-- Specific visualisations
-- TODO Date
-- TODO Place
-- TODO Graph
|]
