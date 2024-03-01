{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Entry.Def where

import Control.Lens.TH (makeLenses)
import Data.Aeson (Value)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (ZonedTime)
import Korrvigs.Geometry
import Korrvigs.Kind (Kind (..))

newtype Id = MkId Text
  deriving (Eq, Ord, Show)

type Metadata = Map Text Value

data Note = MkNote
  { _noteEntry :: Entry,
    _notePath :: Text
  }
  deriving (Show)

data Link = MkLink
  { _linkEntry :: Entry,
    _linkProtocol :: Text,
    _linkRef :: Text
  }
  deriving (Show)

data KindData
  = NoteD Note
  | LinkD Link
  deriving (Show)

class IsKD a where
  toKD :: a -> KindData
  dKind :: a -> Kind
  dEntry :: a -> Entry

instance IsKD Note where
  toKD = NoteD
  dKind = const Note
  dEntry = _noteEntry

instance IsKD Link where
  toKD = LinkD
  dKind = const Link
  dEntry = _linkEntry

instance IsKD KindData where
  toKD = id
  dKind (NoteD _) = Note
  dKind (LinkD _) = Link
  dEntry (NoteD n) = dEntry n
  dEntry (LinkD l) = dEntry l

-- Add lazy cuts in an entry graph
data EntryRef
  = Ref Id
  | Cached Entry
  | NotFound Id
  deriving (Show)

data Entry = MkEntry
  { _name :: Id,
    _date :: Maybe ZonedTime,
    _geo :: Maybe Geometry,
    _metadata :: Metadata,
    _kind :: KindData,
    _children :: [EntryRef]
  }
  deriving (Show)

instance IsKD Entry where
  toKD = _kind
  dKind = dKind . toKD
  dEntry = dEntry . toKD

makeLenses ''Note
makeLenses ''Link
makeLenses ''Entry
