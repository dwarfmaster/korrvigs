{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Calendar.Sync where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (readFile, writeFile)
import Data.List hiding (insert)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Korrvigs.Calendar.SQL
import Korrvigs.Entry
import qualified Korrvigs.Entry.JSON as Gen
import Korrvigs.Kind
import Korrvigs.Monad
import Opaleye (Insert (..), doNothing, rCount, toFields)
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

data CalJSON = CalJSON
  { _cljsServer :: Text,
    _cljsUser :: Text,
    _cljsCalName :: Text,
    _cljsGen :: Gen.EntryJSON
  }

makeLenses ''CalJSON

instance FromJSON CalJSON where
  parseJSON = withObject "CalJSON" $ \v ->
    CalJSON
      <$> v .: "server"
      <*> v .: "user"
      <*> v .: "calendar"
      <*> Gen.parseObject v

instance ToJSON CalJSON where
  toJSON (CalJSON srv user nm gen) =
    object $
      [ "server" .= srv,
        "user" .= user,
        "calendar" .= nm
      ]
        ++ Gen.toObjectPairs gen

calJSONPath :: (MonadKorrvigs m) => m FilePath
calJSONPath = joinPath . (: ["calendars"]) <$> root

calIdFromPath :: FilePath -> Id
calIdFromPath = MkId . T.pack . takeBaseName

calBasename :: Id -> FilePath
calBasename cal = T.unpack $ unId cal <> ".json"

calendarsDirectory :: (MonadKorrvigs m) => m FilePath
calendarsDirectory = calJSONPath

calendarPath' :: (MonadKorrvigs m) => Id -> m FilePath
calendarPath' cal = do
  rt <- calJSONPath
  pure $ joinPath [rt, calBasename cal]

calendarPath :: (MonadKorrvigs m) => Calendar -> m FilePath
calendarPath = calendarPath' . view (calEntry . entryName)

instance Gen.JsonEntry CalJSON Calendar where
  genericJson = cljsGen
  genericKind = const Calendar
  genericUpdateImpl = updateImpl

syncOne :: (MonadKorrvigs m) => Id -> FilePath -> Int -> m SyncData
syncOne i path sqlI = do
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  Gen.syncJsonEntry
    i
    sqlI
    json
    [ let crow = CalRow sqlI (json ^. cljsServer) (json ^. cljsUser) (json ^. cljsCalName) :: CalRow
       in Insert
            { iTable = calendarsTable,
              iRows = [toFields crow],
              iReturning = rCount,
              iOnConflict = Just doNothing
            }
    ]

allCalendars :: (MonadKorrvigs m) => m [FilePath]
allCalendars = do
  rt <- calJSONPath
  files <- liftIO $ listDirectory rt
  pure $ joinPath . (\f -> [rt, f]) <$> files

list :: (MonadKorrvigs m) => m (Set FilePath)
list = S.fromList <$> allCalendars

remove :: (MonadKorrvigs m) => Calendar -> m ()
remove cal = do
  path <- calendarPath cal
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path

updateFile :: (MonadKorrvigs m) => Id -> FilePath -> (CalJSON -> m CalJSON) -> m ()
updateFile i path f = do
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile path $ encodePretty njson

updateImpl :: (MonadKorrvigs m) => Calendar -> (CalJSON -> m CalJSON) -> m ()
updateImpl cal f = do
  path <- calendarPath cal
  let i = cal ^. calEntry . entryName
  updateFile i path f

updateMetadata :: (MonadKorrvigs m) => Calendar -> Map Text Value -> [Text] -> m ()
updateMetadata = Gen.updateMetadata

updateParents :: (MonadKorrvigs m) => Calendar -> [Id] -> [Id] -> m ()
updateParents = Gen.updateParents

updateDate :: (MonadKorrvigs m) => Calendar -> Maybe ZonedTime -> m ()
updateDate = Gen.updateDate

updateDuration :: (MonadKorrvigs m) => Calendar -> Maybe CalendarDiffTime -> m ()
updateDuration = Gen.updateDuration

updateRef :: (MonadKorrvigs m) => Calendar -> Id -> Maybe Id -> m ()
updateRef = Gen.updateRef id

updateTitle :: (MonadKorrvigs m) => Calendar -> Maybe Text -> m ()
updateTitle = Gen.updateTitle
