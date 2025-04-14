{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Calendar.SQL where

import Control.Lens
import Crypto.Hash
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import GHC.Int (Int64)
import Korrvigs.Actions.Utils
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Utils.Crypto ()
import Opaleye

data CalRowImpl a b c d e = CalRow
  { _sqlCalName :: a,
    _sqlCalServer :: b,
    _sqlCalUser :: c,
    _sqlCalCalName :: d,
    _sqlCalCache :: e
  }

makeLenses ''CalRowImpl
$(makeAdaptorAndInstanceInferrable "pCalRow" ''CalRowImpl)

type CalRow = CalRowImpl Id Text Text Text (Digest SHA256)

mkCalRow :: Id -> Text -> Text -> Text -> Digest SHA256 -> CalRow
mkCalRow = CalRow

type CalRowSQL = CalRowImpl (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText)

instance Default ToFields CalRow CalRowSQL where
  def = pCalRow $ CalRow def def def def def

calendarsTable :: Table CalRowSQL CalRowSQL
calendarsTable =
  table "calendars" $
    pCalRow $
      CalRow
        (nameKindField Calendar)
        (tableField "server")
        (tableField "usr")
        (tableField "calname")
        (tableField "cache")

calFromRow :: CalRow -> Entry -> Calendar
calFromRow row entry =
  MkCalendar
    { _calEntry = entry,
      _calServer = row ^. sqlCalServer,
      _calUser = row ^. sqlCalUser,
      _calName = row ^. sqlCalCalName,
      _calCache = row ^. sqlCalCache
    }

sqlLoad :: (MonadKorrvigs m) => Id -> ((Entry -> Calendar) -> Entry) -> m (Maybe Entry)
sqlLoad = genSqlLoad calendarsTable (view sqlCalName) calFromRow

sqlRemove :: Id -> [Delete Int64]
sqlRemove = genSqlRemove calendarsTable $ view sqlCalName
