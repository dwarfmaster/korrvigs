{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Entry.SQL where

import Control.Arrow ((&&&))
import Control.Lens
import Data.Aeson (Result (..), Value, fromJSON)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Profunctor.Product (p2)
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time (CalendarDiffTime, ZonedTime)
import GHC.Int (Int64)
import Korrvigs.Entry.Def
import Korrvigs.Entry.Ident
import Korrvigs.FTS
import Korrvigs.Geometry
import Korrvigs.Kind
import qualified Korrvigs.Utils.Opaleye as Utils
import Opaleye

-- Entries table
data EntryRowImpl a b c d e f = EntryRow
  { _sqlEntryName :: a,
    _sqlEntryKind :: b,
    _sqlEntryDate :: c,
    _sqlEntryDuration :: d,
    _sqlEntryGeo :: e,
    _sqlEntryText :: f
  }

makeLenses ''EntryRowImpl
$(makeAdaptorAndInstanceInferrable "pEntryRow" ''EntryRowImpl)

type EntryRow = EntryRowImpl Id Kind (Maybe ZonedTime) (Maybe CalendarDiffTime) (Maybe Geometry) (Maybe ())

mkEntryRow :: Id -> Kind -> Maybe ZonedTime -> Maybe CalendarDiffTime -> Maybe Geometry -> Maybe () -> EntryRow
mkEntryRow = EntryRow

type EntryRowSQL = EntryRowImpl (Field SqlText) (Field SqlKind) (FieldNullable SqlTimestamptz) (FieldNullable SqlInterval) (FieldNullable SqlGeometry) (FieldNullable SqlTSVector)

instance Default ToFields EntryRow EntryRowSQL where
  def = pEntryRow $ EntryRow def def def def def def

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

-- Metadata table
data MetadataRowImpl a b c = MetadataRow
  { _sqlEntry :: a,
    _sqlKey :: b,
    _sqlValue :: c
  }

makeLenses ''MetadataRowImpl
$(makeAdaptorAndInstanceInferrable "pMetadataRow" ''MetadataRowImpl)

type MetadataRow = MetadataRowImpl Id (CI Text) Value

type MetadataRowSQL = MetadataRowImpl (Field SqlText) (Field SqlText) (Field SqlJsonb)

instance Default ToFields (CI Text) (Field SqlText) where
  def = lmap CI.foldedCase def

instance DefaultFromField SqlText (CI Text) where
  defaultFromField = CI.mk <$> defaultFromField

instance Default ToFields MetadataRow MetadataRowSQL where
  def = pMetadataRow $ MetadataRow def def def

entriesMetadataTable :: Table MetadataRowSQL MetadataRowSQL
entriesMetadataTable =
  table "entries_metadata" $
    pMetadataRow $
      MetadataRow
        (tableField "name")
        (tableField "key")
        (tableField "value")

-- Relation tables
data RelRowImpl a b = RelRow {_source :: a, _target :: b}

makeLenses ''RelRowImpl
$(makeAdaptorAndInstanceInferrable "pRelRow" ''RelRowImpl)

type RelRow = RelRowImpl Id Id

type RelRowSQL = RelRowImpl (Field SqlText) (Field SqlText)

instance Default ToFields RelRow RelRowSQL where
  def = pRelRow $ RelRow def def

entriesSubTable :: Table RelRowSQL RelRowSQL
entriesSubTable =
  table "entries_sub" $ pRelRow $ RelRow (tableField "child") (tableField "parent")

entriesRefTable :: Table RelRowSQL RelRowSQL
entriesRefTable =
  table "entries_ref_to" $ pRelRow $ RelRow (tableField "referer") (tableField "referee")

selectSourcesFor :: Table a RelRowSQL -> Id -> Select (Field SqlText)
selectSourcesFor tbl i =
  Utils.transitiveClosureStep (selectTable tbl) (view target) (view source) (sqlId i)

selectRecSourcesFor :: Table a RelRowSQL -> Id -> Select (Field SqlText)
selectRecSourcesFor tbl i =
  Utils.transitiveClosure (selectTable tbl) (view target) (view source) (sqlId i)

selectTargetsFor :: Table a RelRowSQL -> Id -> Select (Field SqlText)
selectTargetsFor tbl i =
  Utils.transitiveClosureStep (selectTable tbl) (view source) (view target) (sqlId i)

selectRecTargetsFor :: Table a RelRowSQL -> Id -> Select (Field SqlText)
selectRecTargetsFor tbl i =
  Utils.transitiveClosure (selectTable tbl) (view source) (view target) (sqlId i)

-- Helper
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
          _kindData = tkd kd
        }

-- Deal with RelData
insertSubOf :: [(Id, Id)] -> [Insert Int64]
insertSubOf = insertRelationImpl entriesSubTable

insertRefTo :: [(Id, Id)] -> [Insert Int64]
insertRefTo = insertRelationImpl entriesRefTable

insertRelationImpl :: Table RelRowSQL a -> [(Id, Id)] -> [Insert Int64]
insertRelationImpl tbl rels =
  [ Insert
      { iTable = tbl,
        iRows = toFields . uncurry RelRow <$> rels,
        iReturning = rCount,
        iOnConflict = Just doNothing
      }
  ]
