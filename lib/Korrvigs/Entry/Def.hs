{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Entry.Def where

import Control.Lens (Getter, Traversal', to)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson (Value)
import Data.CaseInsensitive (CI)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (CalendarDiffTime, ZonedTime)
import Korrvigs.Entry.Ident
import Korrvigs.Geometry
import Korrvigs.Kind
import Network.Mime (MimeType)

type Metadata = Map (CI Text) Value

data Note = MkNote
  { _noteEntry :: Entry,
    _notePath :: FilePath
  }
  deriving (Show)

data Link = MkLink
  { _linkEntry :: Entry,
    _linkProtocol :: Text,
    _linkRef :: Text,
    _linkPath :: FilePath
  }
  deriving (Show)

data FileStatus
  = FilePlain
  | FilePresent
  | FileAbsent
  deriving (Eq, Show, Enum, Bounded)

displayFileStatus :: FileStatus -> Text
displayFileStatus FilePlain = "plain"
displayFileStatus FilePresent = "present"
displayFileStatus FileAbsent = "absent"

data File = MkFile
  { _fileEntry :: Entry,
    _filePath :: FilePath,
    _fileMeta :: FilePath,
    _fileStatus :: FileStatus,
    _fileMime :: MimeType
  }
  deriving (Show)

data Event = MkEvent
  { _eventEntry :: Entry,
    _eventCalendar :: Text,
    _eventFile :: Text,
    _eventUid :: Text
  }
  deriving (Show)

data Calendar = MkCalendar
  { _calEntry :: Entry,
    _calServer :: Text,
    _calUser :: Text,
    _calName :: Text
  }
  deriving (Show)

data KindData
  = LinkD Link
  | NoteD Note
  | FileD File
  | EventD Event
  | CalendarD Calendar
  deriving (Show)

kindDataKind :: KindData -> Kind
kindDataKind (LinkD _) = Link
kindDataKind (NoteD _) = Note
kindDataKind (FileD _) = File
kindDataKind (EventD _) = Event
kindDataKind (CalendarD _) = Calendar

data Entry = MkEntry
  { _name :: Id,
    _date :: Maybe ZonedTime,
    _duration :: Maybe CalendarDiffTime,
    _geo :: Maybe Geometry,
    _kindData :: KindData
  }
  deriving (Show)

makeLenses ''Calendar
makeLenses ''Event
makeLenses ''Note
makeLenses ''Link
makeLenses ''File
makePrisms ''KindData
makeLenses ''Entry

kind :: Getter Entry Kind
kind = kindData . to kindDataKind

_Note :: Traversal' Entry Note
_Note = kindData . _NoteD

_Link :: Traversal' Entry Link
_Link = kindData . _LinkD

_File :: Traversal' Entry File
_File = kindData . _FileD

_Event :: Traversal' Entry Event
_Event = kindData . _EventD
