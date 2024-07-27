module Korrvigs.Actions.Load where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Link ()
import Korrvigs.Monad
import Korrvigs.Note ()
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
