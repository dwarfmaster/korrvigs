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
    applyCollections,
  )
where

import Control.Arrow (first)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.CaseInsensitive as CI
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Note.AST

data NewEntry = NewEntry
  { _neParents :: [Id],
    _neDate :: Maybe Day,
    _neTitle :: Maybe Text,
    _neLanguage :: Maybe Text,
    _neMtdt :: [(Text, Value)],
    _neCollections :: [(Id, Text)]
  }

makeLenses ''NewEntry

instance Default NewEntry where
  def = NewEntry [] Nothing Nothing Nothing [] []

zonedTimeFromDay :: TimeZone -> Day -> ZonedTime
zonedTimeFromDay tz day =
  ZonedTime (LocalTime day (TimeOfDay 0 0 0)) tz

useDate :: (MonadIO m) => NewEntry -> Maybe ZonedTime -> m (Maybe ZonedTime)
useDate ne dt = do
  tz <- liftIO getCurrentTimeZone
  pure $ mplus (zonedTimeFromDay tz <$> ne ^. neDate) dt

useMtdt :: NewEntry -> Metadata -> Metadata
useMtdt ne = M.union $ M.fromList $ first CI.mk <$> ne ^. neMtdt

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

applyCollections :: (MonadKorrvigs m) => NewEntry -> Id -> m ()
applyCollections ne i =
  forM_ (ne ^. neCollections) $ \(entry, colName) ->
    addToCollection entry colName (ColItemEntry i)
