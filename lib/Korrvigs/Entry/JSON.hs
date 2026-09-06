{-# LANGUAGE FunctionalDependencies #-}

module Korrvigs.Entry.JSON where

import Control.Arrow
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time.LocalTime
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Monad
import Opaleye hiding (not)

data EntryJSON = EntryJSON
  { _ejsMetadata :: Map Text Value,
    _ejsDate :: Maybe ZonedTime,
    _ejsDuration :: Maybe CalendarDiffTime,
    _ejsGeo :: Maybe Geometry,
    _ejsText :: Maybe Text,
    _ejsTitle :: Maybe Text,
    _ejsParents :: [Text]
  }

makeLenses ''EntryJSON

instance Default EntryJSON where
  def = EntryJSON def def def def def def def

parseObject :: Object -> Parser EntryJSON
parseObject obj =
  EntryJSON
    <$> obj .: "metadata"
    <*> obj .:? "date"
    <*> obj .:? "duration"
    <*> obj .:? "geometry"
    <*> obj .:? "textContent"
    <*> obj .:? "title"
    <*> obj .: "parents"

toObjectPairs :: EntryJSON -> [Pair]
toObjectPairs json =
  [ "metadata" .= (json ^. ejsMetadata),
    "parents" .= (json ^. ejsParents)
  ]
    ++ maybe [] (singleton . ("date" .=)) (json ^. ejsDate)
    ++ maybe [] ((: []) . ("duration" .=)) (json ^. ejsDuration)
    ++ maybe [] ((: []) . ("geometry" .=)) (json ^. ejsGeo)
    ++ maybe [] ((: []) . ("textContent" .=)) (json ^. ejsText)
    ++ maybe [] ((: []) . ("title" .=)) (json ^. ejsTitle)

class (ToJSON j, FromJSON j) => JsonEntry j e | j -> e, e -> j where
  genericJson :: Lens' j EntryJSON
  genericKind :: j -> Kind
  genericUpdateImpl :: (MonadKorrvigs m) => e -> (j -> m j) -> m ()

syncJsonEntry :: (MonadKorrvigs m, JsonEntry j e) => Id -> Int -> j -> [Insert Int64] -> m SyncData
syncJsonEntry i sqlI json rows = do
  let mtdt = json ^. genericJson . ejsMetadata
  let tm = json ^. genericJson . ejsDate
  let dur = json ^. genericJson . ejsDuration
  let geom = json ^. genericJson . ejsGeo
  let title = json ^. genericJson . ejsTitle
  let erow = EntryRow (Just sqlI) (genericKind json) i tm dur geom Nothing title :: EntryRowW
  let mtdtrows = first CI.mk <$> M.toList mtdt
  pure $ SyncData erow rows mtdtrows (json ^. genericJson . ejsText) (MkId <$> json ^. genericJson . ejsParents) [] M.empty

updateMetadata :: (MonadKorrvigs m, JsonEntry j e) => e -> Map Text Value -> [Text] -> m ()
updateMetadata e upd rm =
  genericUpdateImpl e $ pure . (genericJson . ejsMetadata %~ M.union upd . flip (foldr M.delete) rm)

updateParents :: (MonadKorrvigs m, JsonEntry j e) => e -> [Id] -> [Id] -> m ()
updateParents e toAdd toRm = genericUpdateImpl e $ pure . updParents
  where
    rmTxt = unId <$> toRm
    addTxt = unId <$> toAdd
    updParents = genericJson . ejsParents %~ (addTxt ++) . filter (not . flip elem rmTxt)

updateDate :: (MonadKorrvigs m, JsonEntry j e) => e -> Maybe ZonedTime -> m ()
updateDate e ntime =
  genericUpdateImpl e $ pure . (genericJson . ejsDate .~ ntime)

updateDuration :: (MonadKorrvigs m, JsonEntry j e) => e -> Maybe CalendarDiffTime -> m ()
updateDuration e ndur =
  genericUpdateImpl e $ pure . (genericJson . ejsDuration .~ ndur)

updateRef :: (MonadKorrvigs m, JsonEntry j e) => (j -> j) -> e -> Id -> Maybe Id -> m ()
updateRef f e old new =
  genericUpdateImpl e $ pure . f . (genericJson . ejsParents %~ upd) . (genericJson . ejsMetadata %~ updateInMetadata old new)
  where
    upd [] = []
    upd (p : ps) | p == unId old = maybe id ((:) . unId) new ps
    upd (p : ps) = p : upd ps

updateTitle :: (MonadKorrvigs m, JsonEntry j e) => e -> Maybe Text -> m ()
updateTitle e ntitle = genericUpdateImpl e $ pure . (genericJson . ejsTitle .~ ntitle)
