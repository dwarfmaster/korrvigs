module Korrvigs.Entry.New
  ( NewEntry (..),
    neParents,
    neDate,
    neTitle,
    neLanguage,
    neMtdt,
    neCollections,
    useDate,
    useMtdt,
    applyNewEntry,
  )
where

import Control.Arrow (first)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Collections

data NewEntry = NewEntry
  { _neParents :: [Id],
    _neDate :: Maybe Day,
    _neTitle :: Maybe Text,
    _neLanguage :: Maybe Text,
    _neMtdt :: [(Text, Value)],
    _neCollections :: Set [Text]
  }

makeLenses ''NewEntry

instance Default NewEntry where
  def = NewEntry [] Nothing Nothing Nothing [] S.empty

zonedTimeFromDay :: TimeZone -> Day -> ZonedTime
zonedTimeFromDay tz day =
  ZonedTime (LocalTime day (TimeOfDay 0 0 0)) tz

useDate :: (MonadIO m) => NewEntry -> Maybe ZonedTime -> m (Maybe ZonedTime)
useDate ne dt = do
  tz <- liftIO getCurrentTimeZone
  pure $ mplus (zonedTimeFromDay tz <$> ne ^. neDate) dt

insertCollections :: [[Text]] -> [(CI Text, Value)] -> [(CI Text, Value)]
insertCollections cols mtdts = case find (\m -> fst m == mtdtName MiscCollection) mtdts of
  Nothing -> (mtdtName MiscCollection, toJSON cols) : mtdts
  Just (_, v) -> case fromJSON v of
    Error _ -> (mtdtName MiscCollection, toJSON cols) : mtdts'
    Success pcols -> (mtdtName MiscCollection, toJSON $ S.toList $ S.fromList $ cols <> pcols) : mtdts'
  where
    mtdts' = filter ((/= mtdtName MiscCollection) . fst) mtdts

useMtdt :: NewEntry -> Metadata -> Metadata
useMtdt ne = M.union $ M.fromList $ addCols $ first CI.mk <$> ne ^. neMtdt
  where
    addCols = case S.toList (ne ^. neCollections) of
      [] -> id
      cols -> insertCollections cols

maybeOrNull :: (b -> Bool) -> a -> (b -> a) -> Maybe b -> a
maybeOrNull _ d _ Nothing = d
maybeOrNull isNull d f (Just x)
  | isNull x = d
  | otherwise = f x

applyNewEntry :: (MonadIO m) => NewEntry -> IdMaker -> m IdMaker
applyNewEntry ne idmk = do
  tz <- liftIO getCurrentTimeZone
  let f = foldr (.) id [setTitle, setDate tz, setParent, setLanguage]
  pure $ f idmk
  where
    setTitle = maybeOrNull T.null id (idTitle ?~) $ ne ^. neTitle
    setDate tz = maybe id ((idDate ?~) . zonedTimeFromDay tz) $ ne ^. neDate
    setParent = maybe id (idParent ?~) $ listToMaybe $ ne ^. neParents
    setLanguage = maybeOrNull T.null id (idLanguage ?~) $ ne ^. neLanguage
