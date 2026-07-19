module Korrvigs.Monad.Syndicate (resolveSyndicate) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Monad.Class
import Korrvigs.Monad.SQL
import Korrvigs.Note hiding (Syndicate)
import Korrvigs.Note.AST (bkNamedSyn)
import Korrvigs.Utils
import Opaleye

resolveSyndicate :: (MonadKorrvigs m) => [(Id, Maybe Text)] -> m [(Id, Maybe Text, Syndicate)]
resolveSyndicate entries = fmap mconcat $ forM entries $ \(i, w) -> case w of
  Nothing -> resolveDirect i
  Just part -> resolveImport i part

resolveDirect :: (MonadKorrvigs m) => Id -> m [(Id, Maybe Text, Syndicate)]
resolveDirect i = do
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

resolveImport :: (MonadKorrvigs m) => Id -> Text -> m [(Id, Maybe Text, Syndicate)]
resolveImport i part = fromMaybeT [] $ do
  entry <- hoistLift $ load i
  note <- hoistMaybe $ entry ^? _Note
  doc <- hoistEither =<< readNote (note ^. notePath)
  syn <- hoistMaybe $ doc ^? docContent . each . bkNamedSyn part
  lift $ resolveSyndicate $ syn ^. _4
