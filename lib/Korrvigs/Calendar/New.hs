module Korrvigs.Calendar.New where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Calendar.Sync
import Korrvigs.Entry
import qualified Korrvigs.Entry.JSON as Gen
import Korrvigs.Entry.New
import Korrvigs.File.New
import Korrvigs.Kind
import Korrvigs.Log
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

new :: (MonadKorrvigs m) => NewCalendar -> m Id
new nc = $withLogContext ("Creating new calendar " <> ncMsg) $ do
  nentry <- applyCover (nc ^. ncEntry) $ Just $ nc ^. ncCalendar
  -- Create ID
  idmk <- applyNewEntry nentry $ imk (choosePrefix PrefixCalendar) & idTitle ?~ nc ^. ncCalendar
  i <- newId idmk
  $logTrace $ NewEntryEvent Calendar i
  -- Make sure directory exists
  dir <- calJSONPath
  liftIO $ createDirectoryIfMissing True dir
  -- Create JSON
  genjson' <- genNewJson nentry
  let genjson = genjson' & Gen.ejsText %~ (((nc ^. ncCalendar <> " ") <>) <$>)
  let json =
        CalJSON
          { _cljsServer = nc ^. ncServer,
            _cljsUser = nc ^. ncUser,
            _cljsCalName = nc ^. ncCalendar,
            _cljsGen = genjson
          }
  path <- calendarPath' i
  $logTrace $ MiscEvent $ "Writing calendar to " <> T.pack path
  writePrettyJsonToFile path json
  -- Sync
  sqlI <- insertNew i Calendar
  syncFileOfKind i path sqlI Calendar
  applyOnNewEntry nentry i
  pure i
  where
    ncMsg = nc ^. ncUser <> "@" <> nc ^. ncServer <> ":" <> nc ^. ncCalendar

moveFile :: (MonadKorrvigs m) => Calendar -> Id -> m ()
moveFile cal ni = do
  oldPath <- calendarPath' $ cal ^. calEntry . entryName
  path <- calendarPath' ni
  liftIO $ BSL.writeFile path =<< BSL.readFile oldPath
  liftIO $ removeFile oldPath
