{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Syndicate.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time.Clock
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad.Class
import Korrvigs.Monad.Utils
import Korrvigs.Utils.Crypto ()
import Opaleye

-- Syndicates table

data SyndicateRowImpl a b c d e f g = SyndicateRow
  { _sqlSynId :: a,
    _sqlSynUrl :: b,
    _sqlSynPath :: c,
    _sqlSynETag :: d,
    _sqlSynFilterEntry :: e,
    _sqlSynFilterCode :: f,
    _sqlSynExpiration :: g
  }

makeLenses ''SyndicateRowImpl
makeAdaptorAndInstanceInferrable "pSynRow" ''SyndicateRowImpl

type SyndicateRow = SyndicateRowImpl Int Text FilePath (Maybe Text) (Maybe Id) (Maybe Text) (Maybe UTCTime)

type SyndicateRowSQL = SyndicateRowImpl (Field SqlInt4) (Field SqlText) (Field SqlText) (FieldNullable SqlText) (FieldNullable SqlText) (FieldNullable SqlText) (FieldNullable SqlTimestamptz)

instance Default ToFields SyndicateRow SyndicateRowSQL where
  def = pSynRow $ SyndicateRow def def def def def def def

syndicatesTable :: Table SyndicateRowSQL SyndicateRowSQL
syndicatesTable =
  table "syndicates" $
    pSynRow $
      SyndicateRow
        (nameKindField Syndicate)
        (tableField "url")
        (tableField "path")
        (tableField "etag")
        (tableField "filter_entry")
        (tableField "filter_code")
        (tableField "expiration")

synFromRow :: SyndicateRow -> Entry -> Syndicate
synFromRow row entry =
  MkSyndicate
    { _synEntry = entry,
      _synUrl = row ^. sqlSynUrl,
      _synPath = row ^. sqlSynPath,
      _synETag = row ^. sqlSynETag,
      _synFilter = (,) <$> row ^. sqlSynFilterEntry <*> row ^. sqlSynFilterCode,
      _synExpiration = row ^. sqlSynExpiration
    }

-- syndicated_items table

data SyndicateItemRowImpl a b c d e f g = SyndicateItemRow
  { _sqlSynItSyndicate :: a,
    _sqlSynItSequence :: b,
    _sqlSynItTitle :: c,
    _sqlSynItUrl :: d,
    _sqlSynItGUID :: e,
    _sqlSynItDate :: f,
    _sqlSynItInstance :: g
  }

makeLenses ''SyndicateItemRowImpl
makeAdaptorAndInstanceInferrable "pSynItRow" ''SyndicateItemRowImpl

type SyndicateItemRow = SyndicateItemRowImpl Int Int Text Text (Maybe Text) (Maybe UTCTime) (Maybe Text)

type SyndicateItemRowSQL = SyndicateItemRowImpl (Field SqlInt4) (Field SqlInt4) (Field SqlText) (Field SqlText) (FieldNullable SqlText) (FieldNullable SqlTimestamptz) (FieldNullable SqlText)

instance Default ToFields SyndicateItemRow SyndicateItemRowSQL where
  def = pSynItRow $ SyndicateItemRow def def def def def def def

syndicatedItemsTable :: Table SyndicateItemRowSQL SyndicateItemRowSQL
syndicatedItemsTable =
  table "syndicated_items" $
    pSynItRow $
      SyndicateItemRow
        (tableField "syndicate")
        (tableField "sequence")
        (tableField "title")
        (tableField "url")
        (tableField "guid")
        (tableField "date")
        (tableField "instance")

-- Functions

sqlLoad :: (MonadKorrvigs m) => Int -> ((Entry -> Syndicate) -> Entry) -> m (Maybe Entry)
sqlLoad = genSqlLoad syndicatesTable (view sqlSynId) synFromRow

sqlRemove :: Int -> [Delete Int64]
sqlRemove i =
  genSqlRemove syndicatedItemsTable (view sqlSynItSyndicate) i
    ++ genSqlRemove syndicatesTable (view sqlSynId) i
