{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Syndicate.Sync where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Default
import Data.Foldable
import Data.List hiding (insert)
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime
import Data.Vector ((!))
import qualified Data.Vector as V
import Korrvigs.Entry
import qualified Korrvigs.Entry.JSON as Gen
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Syndicate.Item
import Korrvigs.Syndicate.SQL
import Korrvigs.Utils (recursiveRemoveFile)
import Korrvigs.Utils.DateTree
import Opaleye hiding (not, null)
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

data SyndicateJSON = SyndicateJSON
  { _synjsUrl :: Maybe Text,
    _synjsETag :: Maybe Text,
    _synjsFilters :: [(Id, Text)],
    _synjsExpiration :: Maybe UTCTime,
    _synjsItems :: [SyndicatedItem],
    _synjsGen :: Gen.EntryJSON
  }

makeLenses ''SyndicateJSON

parseFilter :: Value -> Parser (Id, Text)
parseFilter = withArray "SyndicateJSON filter" $ \arr -> do
  guard $ V.length arr == 2
  i <- parseJSON $ arr ! 0
  code <- parseJSON $ arr ! 1
  pure (MkId i, code)

parseFilters :: Maybe [Value] -> Parser [(Id, Text)]
parseFilters Nothing = pure []
parseFilters (Just vs) = mapM parseFilter vs

instance FromJSON SyndicateJSON where
  parseJSON = withObject "SyndicateJSON" $ \obj ->
    SyndicateJSON
      <$> obj .:? "url"
      <*> obj .:? "etag"
      <*> (obj .:? "filters" >>= parseFilters)
      <*> obj .:? "expiration"
      <*> obj .: "items"
      <*> Gen.parseObject obj

filterToJSON :: (Id, Text) -> Value
filterToJSON (entry, code) = Array $ V.fromList $ toJSON <$> [unId entry, code]

instance ToJSON SyndicateJSON where
  toJSON (SyndicateJSON url etag flt expiration items gen) =
    object $
      ["items" .= items]
        ++ maybe [] ((: []) . ("url" .=)) url
        ++ ["filters" .= (filterToJSON <$> flt) | not (null flt)]
        ++ maybe [] ((: []) . ("etag" .=)) etag
        ++ maybe [] ((: []) . ("expiration" .=)) expiration
        ++ Gen.toObjectPairs gen

synJSONPath :: (MonadKorrvigs m) => m FilePath
synJSONPath = joinPath . (: ["syndicate"]) <$> root

synIdFromPath :: FilePath -> Id
synIdFromPath = MkId . T.pack . takeBaseName

synBasename :: Id -> FilePath
synBasename syn = T.unpack $ unId syn <> ".json"

syndicatesDirectory :: (MonadKorrvigs m) => m FilePath
syndicatesDirectory = synJSONPath

synTreeType :: DateTreeType
synTreeType = def & dtYear .~ True & dtMonth .~ True

remove :: (MonadKorrvigs m) => Syndicate -> m ()
remove syn = do
  let path = syn ^. synPath
  rt <- syndicatesDirectory
  exists <- liftIO $ doesFileExist path
  when exists $ recursiveRemoveFile rt path

allSyndicates :: (MonadKorrvigs m) => m [FilePath]
allSyndicates = do
  rt <- syndicatesDirectory
  let dtt = synTreeType
  files <- listFiles rt dtt
  pure $ view _1 <$> files

list :: (MonadKorrvigs m) => m (Set FilePath)
list = S.fromList <$> allSyndicates

instance Gen.JsonEntry SyndicateJSON Syndicate where
  genericJson = synjsGen
  genericKind = const Syndicate
  genericUpdateImpl = updateImpl

syncOne :: (MonadKorrvigs m) => Id -> FilePath -> Int -> m SyncData
syncOne i path sqlI = do
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  let renderFilter (MkId fId, fCode) = fId <> "#" <> fCode
  let srow = SyndicateRow sqlI (json ^. synjsUrl) path (json ^. synjsETag) (renderFilter <$> json ^. synjsFilters) (json ^. synjsExpiration) :: SyndicateRow
  let insert =
        Insert
          { iTable = syndicatesTable,
            iRows = [toFields srow],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  let irows = flip fmap (zip [1 ..] $ json ^. synjsItems) $ \(sq, item) -> SyndicateItemRow sqlI sq (item ^. synitTitle) (item ^. synitUrl) (item ^. synitRead) (item ^. synitGUID) (item ^. synitDate) :: SyndicateItemRow
  let insertItemRows =
        Insert
          { iTable = syndicatedItemsTable,
            iRows = toFields <$> irows,
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  let refs = json ^.. synjsFilters . each . _1
  dat <- Gen.syncJsonEntry i sqlI json [insert, insertItemRows]
  pure $ dat & syncRefs .~ refs

updateFile :: (MonadKorrvigs m) => Id -> FilePath -> (SyndicateJSON -> m SyndicateJSON) -> m ()
updateFile i path f = do
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  njson <- f json
  liftIO $ writeFile path $ encodePretty njson

updateImpl :: (MonadKorrvigs m) => Syndicate -> (SyndicateJSON -> m SyndicateJSON) -> m ()
updateImpl syn = updateFile (syn ^. synEntry . entryName) (syn ^. synPath)

updateMetadata :: (MonadKorrvigs m) => Syndicate -> Map Text Value -> [Text] -> m ()
updateMetadata = Gen.updateMetadata

updateParents :: (MonadKorrvigs m) => Syndicate -> [Id] -> [Id] -> m ()
updateParents = Gen.updateParents

updateDate :: (MonadKorrvigs m) => Syndicate -> Maybe ZonedTime -> m ()
updateDate = Gen.updateDate

updateDuration :: (MonadKorrvigs m) => Syndicate -> Maybe CalendarDiffTime -> m ()
updateDuration = Gen.updateDuration

updateRef :: (MonadKorrvigs m) => Syndicate -> Id -> Maybe Id -> m ()
updateRef syn old new = Gen.updateRef (synjsFilters %~ (>>= updFilter)) syn old new
  where
    updFilter (entry, code) | entry == old = (,code) <$> toList new
    updFilter (entry, code) = [(entry, code)]

updateTitle :: (MonadKorrvigs m) => Syndicate -> Maybe Text -> m ()
updateTitle = Gen.updateTitle

readItem :: (MonadKorrvigs m) => Syndicate -> Int -> m ()
readItem syn item = do
  let sqlI = syn ^. synEntry . entryId
  updateImpl syn $ pure . (synjsItems . ix (item - 1) . synitRead .~ True)
  atomicSQL $ \conn -> do
    void $
      runUpdate conn $
        Update
          { uTable = syndicatedItemsTable,
            uUpdateWith = sqlSynItRead .~ sqlBool True,
            uWhere = \row -> row ^. sqlSynItSyndicate .== sqlInt4 sqlI .&& (row ^. sqlSynItSequence) .== sqlInt4 item,
            uReturning = rCount
          }
