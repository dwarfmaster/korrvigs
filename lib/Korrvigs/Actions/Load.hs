module Korrvigs.Actions.Load where

import Control.Lens
import qualified Data.Map as M
import Korrvigs.AllEntries ()
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Monad
import Opaleye

mkEntry :: (IsKD a) => f a -> EntryRow -> (Entry -> a) -> Entry
mkEntry _ row = dEntry . entryFromRow dToData row

load :: (MonadKorrvigs m) => Id -> m (Maybe Entry)
load i = do
  mrow <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    pure entry
  case mrow of
    Nothing -> pure Nothing
    Just row -> do
      case row ^. sqlEntryKind of
        Note -> dLoad i $ mkEntry (Nothing :: Maybe Note) row
        Link -> dLoad i $ mkEntry (Nothing :: Maybe Link) row
        File -> dLoad i $ mkEntry (Nothing :: Maybe File) row
        Event -> dLoad i $ mkEntry (Nothing :: Maybe Event) row

loadMetadata :: (MonadKorrvigs m) => Id -> m Metadata
loadMetadata i = do
  mtdt <- rSelect $ do
    mtdtRow <- selectTable entriesMetadataTable
    where_ $ mtdtRow ^. sqlEntry .== sqlId i
    pure (mtdtRow ^. sqlKey, mtdtRow ^. sqlValue)
  pure $ M.fromList mtdt
