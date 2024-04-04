{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Entry.SQL where

import Control.Arrow ((&&&))
import Control.Lens
import Data.Aeson (Result (..), Value, fromJSON)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Profunctor.Product (p2)
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time (CalendarDiffTime, ZonedTime)
import Korrvigs.Entry.Def
import Korrvigs.Entry.Ident
import Korrvigs.FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import Opaleye

-- Entries table
data EntryRowImpl a b c d e f g = EntryRow
  { _sqlEntryName :: a,
    _sqlEntryKind :: b,
    _sqlEntryDate :: c,
    _sqlEntryDuration :: d,
    _sqlEntryGeo :: e,
    _sqlEntryText :: f,
    _sqlEntryMetadata :: g
  }

makeLenses ''EntryRowImpl
$(makeAdaptorAndInstanceInferrable "pEntryRow" ''EntryRowImpl)

type EntryRow = EntryRowImpl Id Kind (Maybe ZonedTime) (Maybe CalendarDiffTime) (Maybe Geometry) (Maybe ()) (Maybe Value)

mkEntryRow :: Id -> Kind -> Maybe ZonedTime -> Maybe CalendarDiffTime -> Maybe Geometry -> Maybe () -> Maybe Value -> EntryRow
mkEntryRow = EntryRow

type EntryRowSQL = EntryRowImpl (Field SqlText) (Field SqlKind) (FieldNullable SqlTimestamptz) (FieldNullable SqlInterval) (FieldNullable SqlGeometry) (FieldNullable SqlTSVector) (FieldNullable SqlJsonb)

instance Default ToFields EntryRow EntryRowSQL where
  def = pEntryRow $ EntryRow def def def def def def def

entriesTable ::
  Table EntryRowSQL EntryRowSQL
entriesTable =
  table "entries" $
    pEntryRow $
      EntryRow
        (tableField "name")
        (tableField "kind")
        (tableField "date")
        (tableField "duration")
        (tableField "geo")
        (tableField "text")
        (tableField "metadata")

nameKindField :: Kind -> TableFields (Field SqlText) (Field SqlText)
nameKindField kd =
  dimap
    (id &&& const (sqlKind kd))
    (^. _1)
    $ p2 (tableField "name", tableField "kind")

mtdtFromJSON :: Maybe Value -> Map Text Value
mtdtFromJSON json = case fromJSON <$> json of
  Just (Success mp) -> mp
  _ -> M.empty

entryFromRow :: (a -> KindData) -> EntryRow -> (Entry -> a) -> a
entryFromRow tkd row cstr = kd
  where
    kd = cstr entry
    entry =
      MkEntry
        { _name = row ^. sqlEntryName,
          _date = row ^. sqlEntryDate,
          _duration = row ^. sqlEntryDuration,
          _geo = row ^. sqlEntryGeo,
          _metadata = mtdtFromJSON $ row ^. sqlEntryMetadata,
          _kindData = tkd kd,
          _children = []
        }
