module Korrvigs.Actions.Load where

import Control.Lens
import Data.Aeson (Value)
import Data.Text (Text)
import Korrvigs.AllEntries ()
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Monad
import Opaleye

mkEntry :: (IsKD a) => f a -> EntryRow -> [(Text, Value, Bool)] -> (Entry -> a) -> Entry
mkEntry _ row mtdt = dEntry . entryFromRow dToData row mtdt

load :: (MonadKorrvigs m) => Id -> m (Maybe Entry)
load i = do
  mrow <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    pure entry
  mtdt <- rSelect $ do
    mtdt <- selectTable entriesMetadataTable
    where_ $ mtdt ^. sqlEntry .== sqlId i
    pure (mtdt ^. sqlKey, mtdt ^. sqlValue, mtdt ^. sqlReadOnly)
  case mrow of
    Nothing -> pure Nothing
    Just row -> do
      case row ^. sqlEntryKind of
        Note -> dLoad i $ mkEntry (Nothing :: Maybe Note) row mtdt
        Link -> dLoad i $ mkEntry (Nothing :: Maybe Link) row mtdt
        File -> dLoad i $ mkEntry (Nothing :: Maybe File) row mtdt
        Event -> dLoad i $ mkEntry (Nothing :: Maybe Event) row mtdt
