module Korrvigs.Entry.New
  ( NewEntry (..),
    neParents,
    neDate,
    neTitle,
    neLanguage,
    neMtdt,
    neCollections,
    neChildren,
    neCover,
    neContent,
    useDate,
    useMtdt,
    applyNewEntry,
    applyCollections,
    applyChildren,
    setMtdtValue,
    setMtdtValueV,
    setMtdtValueM,
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.CaseInsensitive (CI)
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Monad.Metadata (updateParents)
import Korrvigs.Note.AST

data NewEntry = NewEntry
  { _neParents :: [Id],
    _neDate :: Maybe Day,
    _neTitle :: Maybe Text,
    _neLanguage :: Maybe Text,
    _neMtdt :: Map (CI Text) Value,
    _neCollections :: [(Id, Text)],
    _neChildren :: [Id],
    _neCover :: Maybe Text,
    _neContent :: Maybe Text
  }

makeLenses ''NewEntry

instance Default NewEntry where
  def = NewEntry [] Nothing Nothing Nothing M.empty [] [] Nothing Nothing

zonedTimeFromDay :: TimeZone -> Day -> ZonedTime
zonedTimeFromDay tz day =
  ZonedTime (LocalTime day (TimeOfDay 0 0 0)) tz

useDate :: (MonadIO m) => NewEntry -> Maybe ZonedTime -> m (Maybe ZonedTime)
useDate ne dt = do
  tz <- liftIO getCurrentTimeZone
  pure $ mplus (zonedTimeFromDay tz <$> ne ^. neDate) dt

useMtdt :: NewEntry -> Metadata -> Metadata
useMtdt ne = M.union $ ne ^. neMtdt

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

applyChildren :: (MonadKorrvigs m) => NewEntry -> Id -> m ()
applyChildren ne i = do
  forM_ (ne ^. neChildren) $
    load
      >=> ( \case
              Just child -> updateParents child [i] []
              Nothing -> pure ()
          )

setMtdtValue :: (ExtraMetadata mtdt, ToJSON (MtdtType mtdt)) => mtdt -> MtdtType mtdt -> NewEntry -> NewEntry
setMtdtValue mtdt = setMtdtValueV mtdt . toJSON

setMtdtValueV :: (ExtraMetadata mtdt) => mtdt -> Value -> NewEntry -> NewEntry
setMtdtValueV mtdt val = neMtdt . at (mtdtName mtdt) ?~ val

setMtdtValueM :: (ExtraMetadata mtdt, ToJSON (MtdtType mtdt)) => mtdt -> Maybe (MtdtType mtdt) -> NewEntry -> NewEntry
setMtdtValueM _ Nothing = id
setMtdtValueM mtdt (Just val) = setMtdtValue mtdt val
