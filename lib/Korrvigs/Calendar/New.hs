module Korrvigs.Calendar.New where

import Control.Arrow (first)
import Control.Lens
import Control.Monad.IO.Class
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Calendar.JSON
import Korrvigs.Calendar.Sync
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.New
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Monad.Sync (syncFileOfKind)
import Korrvigs.Utils.JSON (writePrettyJsonToFile)
import System.Directory

data NewCalendar = NewCalendar
  { _ncEntry :: NewEntry,
    _ncServer :: Text,
    _ncUser :: Text,
    _ncCalendar :: Text
  }

makeLenses ''NewCalendar

unmapCI :: Map (CI Text) a -> Map Text a
unmapCI = M.fromList . fmap (first CI.foldedCase) . M.toList

new :: (MonadKorrvigs m) => NewCalendar -> m Id
new nc = do
  nentry <- applyCover (nc ^. ncEntry) $ Just $ nc ^. ncCalendar
  -- Create ID
  idmk <- applyNewEntry nentry $ imk "cal" & idTitle ?~ nc ^. ncCalendar
  i <- newId idmk
  -- Make sure directory exists
  dir <- calJSONPath
  liftIO $ createDirectoryIfMissing True dir
  -- Create JSON
  dt <- useDate nentry Nothing
  let json =
        CalJSON
          { _cljsServer = nc ^. ncServer,
            _cljsUser = nc ^. ncUser,
            _cljsCalName = nc ^. ncCalendar,
            _cljsMetadata = unmapCI $ useMtdt nentry M.empty,
            _cljsDate = dt,
            _cljsDuration = Nothing,
            _cljsGeo = Nothing,
            _cljsText = ((nc ^. ncCalendar <> " ") <>) <$> nentry ^. neTitle,
            _cljsTitle = nentry ^. neTitle,
            _cljsParents = unId <$> nentry ^. neParents
          }
  path <- calendarPath' i
  writePrettyJsonToFile path json
  -- Sync
  syncFileOfKind path Calendar
  applyCollections nentry i
  applyChildren nentry i
  applyCapture nentry i
  pure i
