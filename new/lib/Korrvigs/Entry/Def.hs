{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Entry.Def where

import Control.Lens.TH (makeLenses)
import Data.Aeson (Value)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (CalendarDiffTime, ZonedTime)
import Korrvigs.Entry.Ident
import Korrvigs.Geometry
import Korrvigs.Kind (Kind (..))
import Korrvigs.Monad (MonadKorrvigs)

type Metadata = Map Text Value

class IsKD a where
  -- Load from database
  dLoad :: MonadKorrvigs m => Id -> m (Maybe a)

  -- Add a new entry
  data KDMaker a
  dMake :: MonadKorrvigs m => KDMaker a -> m ()

  -- Update an entry
  data KDUpdater a
  dUpdate :: MonadKorrvigs m => KDUpdater a -> m ()

  -- List the ids present in the filesystem
  dList :: MonadKorrvigs m => f a -> m (Set Id)

  -- Sync the content of the filesystem to the database
  dSync :: MonadKorrvigs m => f a -> m ()
  dSyncOne :: MonadKorrvigs m => f a -> Id -> m ()

  -- Query basic information
  dKind :: a -> Kind
  dEntry :: a -> Entry

data Note = MkNote
  { _noteEntry :: Entry,
    _notePath :: Text
  }
  deriving (Show)

instance IsKD Note where
  data KDMaker Note = NoteMaker
  data KDUpdater Note = NoteUpdater
  dLoad = undefined
  dMake = undefined
  dUpdate = undefined
  dList = undefined
  dSync = undefined
  dSyncOne = undefined
  dKind = const Note
  dEntry = _noteEntry

data Link = MkLink
  { _linkEntry :: Entry,
    _linkProtocol :: Text,
    _linkRef :: Text
  }
  deriving (Show)

data KindData
  = LinkD Link
  | NoteD Note
  deriving (Show)

-- Add lazy cuts in an entry graph
data EntryRef
  = Ref Id
  | Cached Entry
  | NotFound Id
  deriving (Show)

data Entry = MkEntry
  { _name :: Id,
    _date :: Maybe ZonedTime,
    _duration :: Maybe CalendarDiffTime,
    _geo :: Maybe Geometry,
    _metadata :: Metadata,
    _kind :: KindData,
    _children :: [EntryRef]
  }
  deriving (Show)

makeLenses ''Note
makeLenses ''Link
makeLenses ''Entry
