{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Compute.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Int (Int64)
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.Type
import Korrvigs.Entry
import Korrvigs.Utils.Opaleye (makeSqlMapper)
import Opaleye
import Opaleye.Experimental.Enum

data SqlRunnableType

sqlRunnableTypeMapper :: EnumMapper SqlRunnableType RunnableType
sqlRunnableTypeMapper = makeSqlMapper "runnabletype" (T.unpack . runTypeName)

instance DefaultFromField SqlRunnableType RunnableType where
  defaultFromField = enumFromField sqlRunnableTypeMapper

instance Default ToFields RunnableType (Field SqlRunnableType) where
  def = enumToFields sqlRunnableTypeMapper

sqlRunnableType :: RunnableType -> Field SqlRunnableType
sqlRunnableType = toFields

data ComputationResult = ComputationResult
  { _cmpResType :: RunnableType,
    _cmpResHash :: Hash,
    _cmpResDate :: UTCTime,
    _cmpResRuntime :: Int, -- In milliseconds
    _cmpResData :: RunnableResult
  }
  deriving (Eq, Show)

data Computation = Computation
  { _cmpEntry :: Id,
    _cmpName :: Text,
    _cmpRun :: Runnable,
    _cmpResult :: Maybe ComputationResult,
    _cmpAutorun :: Maybe Text
  }
  deriving (Eq, Show)

data CompRowImpl a b c d e f = CompRow
  { _sqlCompEntry :: a,
    _sqlCompName :: b,
    _sqlCompType :: c,
    _sqlCompAutorun :: d,
    _sqlCompLastRun :: e,
    _sqlCompRunTime :: f
  }

data CompDepRowImpl a b c d = CompDepRow
  { _sqlCompDepSrcEntry :: a,
    _sqlCompDepSrcName :: b,
    _sqlCompDepDstEntry :: c,
    _sqlCompDepDstName :: d
  }

makeLenses ''ComputationResult
makeLenses ''Computation
makeLenses ''CompRowImpl
makeLenses ''CompDepRowImpl
$(makeAdaptorAndInstanceInferrable "pCompRow" ''CompRowImpl)
$(makeAdaptorAndInstanceInferrable "pCompDepRow" ''CompDepRowImpl)

type CompRow = CompRowImpl Int Text RunnableType (Maybe Text) (Maybe UTCTime) (Maybe Int)

type CompRowSQL = CompRowImpl (Field SqlInt4) (Field SqlText) (Field SqlRunnableType) (FieldNullable SqlText) (FieldNullable SqlTimestamptz) (FieldNullable SqlInt4)

instance Default ToFields CompRow CompRowSQL where
  def = pCompRow $ CompRow def def def def def def

computationsTable :: Table CompRowSQL CompRowSQL
computationsTable =
  table "computations" $
    pCompRow $
      CompRow
        (tableField "entry")
        (tableField "name")
        (tableField "type")
        (tableField "autorun")
        (tableField "last_run")
        (tableField "run_time")

type CompDepRow = CompDepRowImpl Int Text Int Text

type CompDepRowSQL = CompDepRowImpl (Field SqlInt4) (Field SqlText) (Field SqlInt4) (Field SqlText)

instance Default ToFields CompDepRow CompDepRowSQL where
  def = pCompDepRow $ CompDepRow def def def def

computationsDepTable :: Table CompDepRowSQL CompDepRowSQL
computationsDepTable =
  table "computations_dep" $
    pCompDepRow $
      CompDepRow
        (tableField "entry")
        (tableField "name")
        (tableField "entry_dep")
        (tableField "name_dep")

selComp :: Field SqlInt4 -> Text -> Select CompRowSQL
selComp i nm = do
  cmp <- selectTable computationsTable
  where_ $ cmp ^. sqlCompEntry .== i
  where_ $ cmp ^. sqlCompName .== sqlStrictText nm
  pure cmp

insertCompDeps :: [CompDepRow] -> Insert Int64
insertCompDeps deps =
  Insert
    { iTable = computationsDepTable,
      iRows = toFields <$> deps,
      iReturning = rCount,
      iOnConflict = Just doNothing
    }
