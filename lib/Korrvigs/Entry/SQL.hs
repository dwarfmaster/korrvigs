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
data EntryRowImpl a b c d e f g h = EntryRow
  { _sqlEntryId :: a,
    _sqlEntryKind :: b,
    _sqlEntryName :: c,
    _sqlEntryDate :: d,
    _sqlEntryDuration :: e,
    _sqlEntryGeo :: f,
    _sqlEntryText :: g,
    _sqlEntryTitle :: h
  }

makeLenses ''EntryRowImpl
$(makeAdaptorAndInstanceInferrable "pEntryRow" ''EntryRowImpl)

type EntryRowR = EntryRowImpl Int Kind Id (Maybe ZonedTime) (Maybe CalendarDiffTime) (Maybe Geometry) (Maybe ()) (Maybe Text)

type EntryRowW = EntryRowImpl (Maybe Int) Kind Id (Maybe ZonedTime) (Maybe CalendarDiffTime) (Maybe Geometry) (Maybe ()) (Maybe Text)

mkEntryRow :: Maybe Int -> Kind -> Id -> Maybe ZonedTime -> Maybe CalendarDiffTime -> Maybe Geometry -> Maybe () -> Maybe Text -> EntryRowW
mkEntryRow = EntryRow

type EntryRowSQLR = EntryRowImpl (Field SqlInt4) (Field SqlKind) (Field SqlText) (FieldNullable SqlTimestamptz) (FieldNullable SqlInterval) (FieldNullable SqlGeometry) (FieldNullable SqlTSVector) (FieldNullable SqlText)

type EntryRowSQLW = EntryRowImpl (Maybe (Field SqlInt4)) (Field SqlKind) (Field SqlText) (FieldNullable SqlTimestamptz) (FieldNullable SqlInterval) (FieldNullable SqlGeometry) (FieldNullable SqlTSVector) (FieldNullable SqlText)

instance Default ToFields EntryRowW EntryRowSQLW where
  def = pEntryRow $ EntryRow def def def def def def def def

entriesTable ::
  Table EntryRowSQLW EntryRowSQLR
entriesTable =
  table "entries" $
    pEntryRow $
      EntryRow
        (optionalTableField "id")
        (tableField "kind")
        (tableField "name")
        (tableField "date")
        (tableField "duration")
        (tableField "geo")
        (tableField "text")
        (tableField "title")

fromName :: (Field SqlInt4 -> Select a) -> Field SqlText -> Select a
fromName query i = do
  entry <- selectTable entriesTable
  where_ $ entry ^. sqlEntryName .== i
  query $ entry ^. sqlEntryId

nameFor :: Field SqlInt4 -> Select (Field SqlText)
nameFor i = do
  entry <- selectTable entriesTable
  where_ $ entry ^. sqlEntryId .== i
  pure $ entry ^. sqlEntryName

nameKindField :: Kind -> TableFields (Field SqlInt4) (Field SqlInt4)
nameKindField kd =
  dimap
    (id &&& const (sqlKind kd))
    (^. _1)
    $ p2 (tableField "id", tableField "kind")

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

type MetadataRow = MetadataRowImpl Int (CI Text) Value

type MetadataRowSQL = MetadataRowImpl (Field SqlInt4) (Field SqlText) (Field SqlJsonb)

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
        (tableField "entry")
        (tableField "key")
        (tableField "value")

-- Relation tables
data RelRowImpl a b = RelRow {_source :: a, _target :: b}

makeLenses ''RelRowImpl
$(makeAdaptorAndInstanceInferrable "pRelRow" ''RelRowImpl)

type RelRow = RelRowImpl Int Int

type RelRowSQL = RelRowImpl (Field SqlInt4) (Field SqlInt4)

instance Default ToFields RelRow RelRowSQL where
  def = pRelRow $ RelRow def def

entriesSubTable :: Table RelRowSQL RelRowSQL
entriesSubTable =
  table "entries_sub" $ pRelRow $ RelRow (tableField "child") (tableField "parent")

entriesRefTable :: Table RelRowSQL RelRowSQL
entriesRefTable =
  table "entries_ref_to" $ pRelRow $ RelRow (tableField "referer") (tableField "referee")

selectSourcesFor :: Table a RelRowSQL -> Field SqlInt4 -> Select (Field SqlInt4)
selectSourcesFor tbl =
  Utils.transitiveClosureStep (selectTable tbl) (view target) (view source)

selectRecSourcesFor :: Table a RelRowSQL -> Field SqlInt4 -> Select (Field SqlInt4)
selectRecSourcesFor tbl =
  Utils.transitiveClosure (selectTable tbl) (view target) (view source)

selectTargetsFor :: Table a RelRowSQL -> Field SqlInt4 -> Select (Field SqlInt4)
selectTargetsFor tbl =
  Utils.transitiveClosureStep (selectTable tbl) (view source) (view target)

selectRecTargetsFor :: Table a RelRowSQL -> Field SqlInt4 -> Select (Field SqlInt4)
selectRecTargetsFor tbl =
  Utils.transitiveClosure (selectTable tbl) (view source) (view target)

-- Helper
entryFromRow :: (a -> KindData) -> EntryRowR -> (Entry -> a) -> a
entryFromRow tkd row cstr = kd
  where
    kd = cstr entry
    entry =
      MkEntry
        { _entryId = row ^. sqlEntryId,
          _entryDate = row ^. sqlEntryDate,
          _entryName = row ^. sqlEntryName,
          _entryDuration = row ^. sqlEntryDuration,
          _entryGeo = row ^. sqlEntryGeo,
          _entryTitle = row ^. sqlEntryTitle,
          _entryKindData = tkd kd
        }

-- Deal with RelData
insertSubOf :: [(Int, Int)] -> Insert Int64
insertSubOf = insertRelationImpl entriesSubTable

insertRefTo :: [(Int, Int)] -> Insert Int64
insertRefTo = insertRelationImpl entriesRefTable

insertRelationImpl :: Table RelRowSQL a -> [(Int, Int)] -> Insert Int64
insertRelationImpl tbl rels =
  Insert
    { iTable = tbl,
      iRows = toFields . uncurry RelRow <$> rels,
      iReturning = rCount,
      iOnConflict = Just doNothing
    }
