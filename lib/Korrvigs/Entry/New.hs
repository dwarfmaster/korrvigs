module Korrvigs.Entry.New
  ( NewEntry (..),
    neParents,
    neDate,
    neTitle,
    neLanguage,
    neMtdt,
    newExtraMtdt,
    applyNewEntry,
  )
where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.LocalTime
import Korrvigs.Entry.Ident
import Korrvigs.Metadata

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

nullToNothing :: [a] -> Maybe [a]
nullToNothing [] = Nothing
nullToNothing l = Just l

newExtraMtdt :: (MonadIO m) => NewEntry -> m MtdtExtras
newExtraMtdt ne = do
  tz <- liftIO getCurrentTimeZone
  pure $
    def
      & mtdtTitle .~ ne ^. neTitle
      & mtdtDate .~ (zonedTimeFromDay tz <$> ne ^. neDate)
      & mtdtParents .~ ne ^. neParents . to nullToNothing

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
