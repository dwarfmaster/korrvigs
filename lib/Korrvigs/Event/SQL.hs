{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Event.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad.Class
import Korrvigs.Monad.Utils
import Opaleye

data EventRowImpl a b c d = EventRow
  { _sqlEventId :: a,
    _sqlEventCalendar :: b,
    _sqlEventFile :: c,
    _sqlEventUID :: d
  }

makeLenses ''EventRowImpl
$(makeAdaptorAndInstanceInferrable "pEventRow" ''EventRowImpl)

type EventRow = EventRowImpl Int Id FilePath Text

mkEventRow :: Int -> Id -> FilePath -> Text -> EventRow
mkEventRow = EventRow

type EventRowSQL = EventRowImpl (Field SqlInt4) (Field SqlText) (Field SqlText) (Field SqlText)

instance Default ToFields EventRow EventRowSQL where
  def = pEventRow $ EventRow def def def def

eventsTable :: Table EventRowSQL EventRowSQL
eventsTable =
  table "events" $
    pEventRow $
      EventRow
        (nameKindField Event)
        (tableField "calendar")
        (tableField "file")
        (tableField "uid")

eventFromRow :: EventRow -> Entry -> Event
eventFromRow erow entry = MkEvent entry (erow ^. sqlEventCalendar) (erow ^. sqlEventFile) (erow ^. sqlEventUID)

sqlLoad :: (MonadKorrvigs m) => Int -> ((Entry -> Event) -> Entry) -> m (Maybe Entry)
sqlLoad = genSqlLoad eventsTable (view sqlEventId) eventFromRow

sqlRemove :: Int -> [Delete Int64]
sqlRemove = genSqlRemove eventsTable $ view sqlEventId
