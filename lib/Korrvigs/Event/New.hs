module Korrvigs.Event.New where

import Conduit (throwM)
import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson (toJSON)
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Format
import Data.Time.LocalTime
import Korrvigs.Calendar.SQL
import Korrvigs.Entry.Ident
import Korrvigs.Entry.New
import Korrvigs.Entry.SQL
import Korrvigs.Event.ICalendar
import Korrvigs.Event.Sync
import Korrvigs.File.New
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Monad.Sync
import Korrvigs.Utils.DateTree
import Opaleye hiding (not, null)

data NewEvent = NewEvent
  { _nevEntry :: NewEntry,
    _nevCalendar :: Id,
    _nevStart :: LocalTime,
    _nevEnd :: LocalTime,
    _nevSummary :: Text,
    _nevDescription :: Maybe Text,
    _newLocation :: Maybe Text,
    _nevOpaque :: Bool
  }

makeLenses ''NewEvent

new :: (MonadKorrvigs m) => NewEvent -> m Id
new opts = do
  calExists <- fmap (not . null) $ rSelectOne $ do
    cal <- selectTable calendarsTable
    nm <- nameFor $ cal ^. sqlCalId
    where_ $ nm .== sqlId (opts ^. nevCalendar)
  unless calExists $ throwM $ KMiscError $ "Calendar \"" <> unId (opts ^. nevCalendar) <> "\" does not exists"
  rt <- eventsDirectory
  tz <- liftIO getCurrentTimeZone
  let tzname = if null (timeZoneName tz) then "KorrvigsTZ" else T.pack (timeZoneName tz)
  let ictz =
        ICTZSpec
          { _ictzStandard = not $ timeZoneSummerOnly tz,
            _ictzStart = LocalTime (fromOrdinalDate 1900 1) (TimeOfDay 0 0 0),
            _ictzOffsetFrom = 0,
            _ictzOffsetTo = timeZoneMinutes tz * 60,
            _ictzRdate = Nothing,
            _ictzName = if tzname == "KorrvigsTZ" then Nothing else Just tzname,
            _ictzRRule = Nothing,
            _ictzContent = def
          }
  localTime <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
  let stamp = formatTime defaultTimeLocale "%0Y%m%dT%H%M%S" localTime
  let title = fromMaybe (opts ^. nevSummary) $ opts ^. nevEntry . neTitle
  nentry <- applyCover (opts ^. nevEntry) $ Just title
  let ievent =
        ICEvent
          { _iceUid = "",
            _iceCategories = [],
            _iceComment = Nothing,
            _iceSummary = Just title,
            _iceDescription = opts ^. nevDescription,
            _iceLocation = opts ^. newLocation,
            _iceStart =
              Just $
                ICTmSpec
                  { _ictmDate = opts ^. nevStart,
                    _ictmUTC = False,
                    _ictmTimeZone = Just tzname
                  },
            _iceEnd =
              Just $
                ICTmSpec
                  { _ictmDate = opts ^. nevEnd,
                    _ictmUTC = False,
                    _ictmTimeZone = Just tzname
                  },
            _iceDuration = Nothing,
            _iceTransparent = not $ opts ^. nevOpaque,
            _iceParents = nentry ^. neParents,
            _iceGeometry = Nothing,
            _iceMtdt =
              mconcat
                [ nentry ^. neMtdt,
                  maybe M.empty (M.singleton (mtdtName Language) . toJSON) (nentry ^. neLanguage)
                ],
            _iceContent =
              def
                & icValues . at "DTSTAMP" ?~ [ICValue M.empty (T.pack stamp)]
          }
  let ical =
        ICFile
          { _icVersion = "2.0",
            _icContent =
              def
                & icValues . at "PRODID" ?~ [ICValue M.empty "/korrvigs/"],
            _icTimezones =
              M.singleton tzname $
                ICTZ
                  { _ictzId = tzname,
                    _ictzSpecs = [ictz],
                    _ictzTopLevel = def
                  },
            _icEvent = Just ievent
          }
  i <- createIdFor ical ievent
  let uid = T.map (\c -> if c == ':' then '-' else c) (unId i) <> "@korrvigs"
  let ncal = ical & icEvent . _Just . iceUid .~ uid
  let filename = unId i <> "_" <> unId (opts ^. nevCalendar) <> ".ics"
  let day = localDay . zonedTimeToLocalTime . resolveICalTime ncal <$> ncal ^? icEvent . _Just . iceStart . _Just
  path <- storeFile rt eventTreeType day filename $ FileLazy $ renderICalFile ncal
  syncFileOfKind path Event
  applyOnNewEntry nentry i
  pure i
