module Korrvigs.Monad.Syndicate where

import Control.Lens
import Control.Monad
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Monad.Class
import Korrvigs.Monad.SQL
import Opaleye

resolveSyndicate :: (MonadKorrvigs m) => [Id] -> m [(Id, Maybe Text, Syndicate)]
resolveSyndicate entries = fmap mconcat $ forM entries $ \i -> do
  syn <- loadSelect $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    syn <- optional $ limit 1 $ do
      where_ $ entry ^. sqlEntryKind ./= sqlKind Syndicate
      syn <- baseSelectTextMtdt SyndicateMtdt $ entry ^. sqlEntryId
      synE <- selectTable entriesTable
      where_ $ synE ^. sqlEntryName .== syn
      pure synE
    pure (entry ^. sqlEntryTitle, fromMaybeFields entry syn)
  pure $ case syn of
    Nothing -> []
    Just (title, s) -> maybe [] (\sn -> [(i, title, sn)]) $ s ^? _Syndicate
