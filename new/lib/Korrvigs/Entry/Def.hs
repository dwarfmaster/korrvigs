{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Entry.Def where

import Control.Lens (Getter, to)
import Control.Lens.TH (makeLenses)
import Data.Aeson (Value)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (CalendarDiffTime, ZonedTime)
import Korrvigs.Entry.Ident
import Korrvigs.Geometry
import Korrvigs.Kind

type Metadata = Map Text Value

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

data KindData
  = LinkD Link
  | NoteD Note
  deriving (Show)

kindDataKind :: KindData -> Kind
kindDataKind (LinkD _) = Link
kindDataKind (NoteD _) = Note

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
makeLenses ''Entry

kind :: Getter Entry Kind
kind = kindData . to kindDataKind
