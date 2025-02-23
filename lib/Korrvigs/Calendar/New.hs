module Korrvigs.Calendar.New where

import Control.Arrow (first)
import Control.Lens
import Control.Monad.IO.Class
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Actions.Sync (processRelData)
import Korrvigs.Calendar.JSON
import Korrvigs.Calendar.Sync
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.Monad
import Korrvigs.Utils.JSON (writeJsonToFile)
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
  -- Create ID
  idmk <- applyNewEntry (nc ^. ncEntry) $ imk "cal" & idTitle ?~ nc ^. ncCalendar
  i <- newId idmk
  -- Make sure directory exists
  dir <- calJSONPath
  liftIO $ createDirectoryIfMissing True dir
  -- Create JSON
  dt <- useDate (nc ^. ncEntry) Nothing
  let json =
        CalJSON
          { _cljsServer = nc ^. ncServer,
            _cljsUser = nc ^. ncUser,
            _cljsCalName = nc ^. ncCalendar,
            _cljsMetadata = unmapCI $ useMtdt (nc ^. ncEntry) M.empty,
            _cljsDate = dt,
            _cljsDuration = Nothing,
            _cljsGeo = Nothing,
            _cljsText = ((nc ^. ncCalendar <> " ") <>) <$> nc ^. ncEntry . neTitle,
            _cljsParents = unId <$> nc ^. ncEntry . neParents
          }
  path <- calendarPath' i
  writeJsonToFile path json
  -- Sync
  rel <- dSyncOneImpl path
  processRelData i rel
  pure i
