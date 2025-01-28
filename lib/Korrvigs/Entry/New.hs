module Korrvigs.Entry.New
  ( NewEntry (..),
    neParents,
    neDate,
    neTitle,
    neLanguage,
    neMtdt,
    useDate,
    useMtdt,
    applyNewEntry,
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.LocalTime
import Korrvigs.Entry

data NewEntry = NewEntry
  { _neParents :: [Id],
    _neDate :: Maybe Day,
    _neTitle :: Maybe Text,
    _neLanguage :: Maybe Text,
    _neMtdt :: [(Text, Value)]
  }

makeLenses ''NewEntry

instance Default NewEntry where
  def = NewEntry [] Nothing Nothing Nothing []

zonedTimeFromDay :: TimeZone -> Day -> ZonedTime
zonedTimeFromDay tz day =
  ZonedTime (LocalTime day (TimeOfDay 0 0 0)) tz

useDate :: (MonadIO m) => NewEntry -> Maybe ZonedTime -> m (Maybe ZonedTime)
useDate ne dt = do
  tz <- liftIO getCurrentTimeZone
  pure $ mplus (zonedTimeFromDay tz <$> ne ^. neDate) dt

useMtdt :: NewEntry -> Metadata -> Metadata
useMtdt ne = M.union $ M.fromList $ ne ^. neMtdt

applyNewEntry :: (MonadIO m) => NewEntry -> IdMaker -> m IdMaker
applyNewEntry ne idmk = do
  tz <- liftIO getCurrentTimeZone
  let f = foldr (.) id [setTitle, setDate tz, setParent, setLanguage]
  pure $ f idmk
  where
    setTitle = maybe id (idTitle ?~) $ ne ^. neTitle
    setDate tz = maybe id ((idDate ?~) . zonedTimeFromDay tz) $ ne ^. neDate
    setParent = maybe id (idParent ?~) $ listToMaybe $ ne ^. neParents
    setLanguage = maybe id (idLanguage ?~) $ ne ^. neLanguage
