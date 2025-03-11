module Korrvigs.Actions.SQL (load, loadMetadata) where

import Control.Lens
import qualified Data.Map as M
import qualified Korrvigs.Calendar.SQL as Cal
import Korrvigs.Entry
import qualified Korrvigs.Event.SQL as Event
import qualified Korrvigs.File.SQL as File
import Korrvigs.Kind
import qualified Korrvigs.Link.SQL as Link
import Korrvigs.Monad
import qualified Korrvigs.Note.SQL as Note
import Opaleye

mkEntry :: (IsKindData a) => EntryRow -> (Entry -> a) -> Entry
mkEntry row = kdEntry . entryFromRow kdKindData row

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
        Note -> Note.sqlLoad i $ mkEntry row
        Link -> Link.sqlLoad i $ mkEntry row
        File -> File.sqlLoad i $ mkEntry row
        Event -> Event.sqlLoad i $ mkEntry row
        Calendar -> Cal.sqlLoad i $ mkEntry row

loadMetadata :: (MonadKorrvigs m) => Id -> m Metadata
loadMetadata i = do
  mtdt <- rSelect $ do
    mtdtRow <- selectTable entriesMetadataTable
    where_ $ mtdtRow ^. sqlEntry .== sqlId i
    pure (mtdtRow ^. sqlKey, mtdtRow ^. sqlValue)
  pure $ M.fromList mtdt
