{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Event.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Kind
import Opaleye

data EventRowImpl a b c d = EventRow
  { _sqlEventName :: a,
    _sqlEventCalendar :: b,
    _sqlEventFile :: c,
    _sqlEventUID :: d
  }

makeLenses ''EventRowImpl
$(makeAdaptorAndInstanceInferrable "pEventRow" ''EventRowImpl)

type EventRow = EventRowImpl Id Text FilePath Text

mkEventRow :: Id -> Text -> FilePath -> Text -> EventRow
mkEventRow = EventRow

type EventRowSQL = EventRowImpl (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText)

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
