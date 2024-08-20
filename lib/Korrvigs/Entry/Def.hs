{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Entry.Def where

import Control.Lens (Getter, Traversal', to, view)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson (Value)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time (CalendarDiffTime, ZonedTime)
import Korrvigs.Entry.Ident
import Korrvigs.Geometry
import Korrvigs.Kind
import Network.Mime (MimeType)

data MetadataValue = MValue
  { _metaValue :: Value,
    _metaReadOnly :: Bool
  }
  deriving (Show, Eq)

makeLenses ''MetadataValue

type Metadata = Map Text MetadataValue

metaLookup :: Text -> Metadata -> Maybe Value
metaLookup key mtdt = view metaValue <$> M.lookup key mtdt

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

data KindData
  = LinkD Link
  | NoteD Note
  | FileD File
  deriving (Show)

kindDataKind :: KindData -> Kind
kindDataKind (LinkD _) = Link
kindDataKind (NoteD _) = Note
kindDataKind (FileD _) = File

data Entry = MkEntry
  { _name :: Id,
    _date :: Maybe ZonedTime,
    _duration :: Maybe CalendarDiffTime,
    _geo :: Maybe Geometry,
    _metadata :: Metadata,
    _kindData :: KindData
  }
  deriving (Show)

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
