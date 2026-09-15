{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.AddressBook.Sync where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.ByteString.Lazy as BSL
import Data.List hiding (insert)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Korrvigs.AddressBook.SQL
import Korrvigs.Entry
import qualified Korrvigs.Entry.JSON as Gen
import Korrvigs.Kind
import Korrvigs.Monad
import Opaleye (Insert (..), doNothing, rCount, toFields)
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

data AbookJSON = AbookJSON
  { _abjsServer :: Text,
    _abjsUser :: Text,
    _abjsName :: Text,
    _abjsGen :: Gen.EntryJSON
  }

makeLenses ''AbookJSON

instance FromJSON AbookJSON where
  parseJSON = withObject "AbookJSON" $ \v ->
    AbookJSON
      <$> v .: "server"
      <*> v .: "user"
      <*> v .: "name"
      <*> Gen.parseObject v

instance ToJSON AbookJSON where
  toJSON (AbookJSON server user abookname gen) =
    object $
      [ "server" .= server,
        "user" .= user,
        "name" .= abookname
      ]
        ++ Gen.toObjectPairs gen

abookJSONPath :: (MonadKorrvigs m) => m FilePath
abookJSONPath = joinPath . (: ["addressbooks"]) <$> root

abookIdFromPath :: FilePath -> Id
abookIdFromPath = MkId . T.pack . takeBaseName

abookBasename :: Id -> FilePath
abookBasename abook = T.unpack $ unId abook <> ".json"

addressbooksDirectory :: (MonadKorrvigs m) => m FilePath
addressbooksDirectory = abookJSONPath

abookPath' :: (MonadKorrvigs m) => Id -> m FilePath
abookPath' abook = do
  rt <- abookJSONPath
  pure $ joinPath [rt, abookBasename abook]

abookPath :: (MonadKorrvigs m) => AddressBook -> m FilePath
abookPath = abookPath' . view (abookEntry . entryName)

instance Gen.JsonEntry AbookJSON AddressBook where
  genericJson = abjsGen
  genericKind = const AddressBook
  genericUpdateImpl = updateImpl

syncOne :: (MonadKorrvigs m) => Id -> FilePath -> Int -> m SyncData
syncOne i path sqlI = do
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  Gen.syncJsonEntry
    i
    sqlI
    json
    [ let abrow = AddressBookRow sqlI (json ^. abjsServer) (json ^. abjsUser) (json ^. abjsName) :: AddressBookRow
       in Insert
            { iTable = addressBooksTable,
              iRows = [toFields abrow],
              iReturning = rCount,
              iOnConflict = Just doNothing
            }
    ]

allAddressBooks :: (MonadKorrvigs m) => m [FilePath]
allAddressBooks = do
  rt <- abookJSONPath
  files <- liftIO $ listDirectory rt
  pure $ joinPath . (\f -> [rt, f]) <$> files

list :: (MonadKorrvigs m) => m (Set FilePath)
list = S.fromList <$> allAddressBooks

remove :: (MonadKorrvigs m) => AddressBook -> m ()
remove abook = do
  path <- abookPath abook
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path

updateFile :: (MonadKorrvigs m) => Id -> FilePath -> (AbookJSON -> m AbookJSON) -> m ()
updateFile i path f = do
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile path $ encodePretty njson

updateImpl :: (MonadKorrvigs m) => AddressBook -> (AbookJSON -> m AbookJSON) -> m ()
updateImpl abook f = do
  path <- abookPath abook
  let i = abook ^. abookEntry . entryName
  updateFile i path f

moveFile :: (MonadKorrvigs m) => AddressBook -> Id -> m ()
moveFile abook ni = do
  oldPath <- abookPath' $ abook ^. abookEntry . entryName
  path <- abookPath' ni
  liftIO $ BSL.writeFile path =<< BSL.readFile oldPath
  liftIO $ removeFile oldPath

updateMetadata :: (MonadKorrvigs m) => AddressBook -> Map Text Value -> [Text] -> m ()
updateMetadata = Gen.updateMetadata

updateParents :: (MonadKorrvigs m) => AddressBook -> [Id] -> [Id] -> m ()
updateParents = Gen.updateParents

updateDate :: (MonadKorrvigs m) => AddressBook -> Maybe ZonedTime -> m ()
updateDate = Gen.updateDate

updateDuration :: (MonadKorrvigs m) => AddressBook -> Maybe CalendarDiffTime -> m ()
updateDuration = Gen.updateDuration

updateRef :: (MonadKorrvigs m) => AddressBook -> Id -> Maybe Id -> m ()
updateRef = Gen.updateRef id

updateTitle :: (MonadKorrvigs m) => AddressBook -> Maybe Text -> m ()
updateTitle = Gen.updateTitle
