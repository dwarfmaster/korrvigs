module Korrvigs.Monad.Metadata.Hooks where

import Control.Lens hiding (op, (.>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Monad.Class
import Korrvigs.Monad.SQL
import Korrvigs.Syndicate.SQL
import Korrvigs.Utils
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye
import Opaleye

type UpdateMtdtFn m = Entry -> Map Text Value -> [Text] -> m ()

updateAggregateFor :: (MonadKorrvigs m) => UpdateMtdtFn m -> Entry -> m ()
updateAggregateFor updateMetadata entry = fromMaybeT () $ do
  synId <- hoistLift $ rSelectTextMtdt SyndicateMtdt $ sqlId $ entry ^. entryName
  synE <- hoistLift $ load $ MkId synId
  syn <- hoistMaybe $ synE ^? _Syndicate
  lift $ updateAggregate updateMetadata entry syn

updateAggregate :: (MonadKorrvigs m) => UpdateMtdtFn m -> Entry -> Syndicate -> m ()
updateAggregate updateMetadata entry syn =
  rSelectTextMtdt AggregateMethod (sqlId $ entry ^. entryName) >>= \case
    Just "count-since-last" -> do
      mseq <- rSelectOne $ do
        (mtdtKey, isStrict) <-
          values
            [ (sqlStrictText $ mtdtSqlName LastRead, sqlBool True),
              (sqlStrictText $ mtdtSqlName FirstUnread, sqlBool False)
            ]
        mtdt <- selectTable entriesMetadataTable
        where_ $ mtdt ^. sqlEntry .== sqlInt4 sqlI
        where_ $ mtdt ^. sqlKey .== mtdtKey
        url <- fromNullableSelect $ pure $ sqlJsonToText $ toNullable $ mtdt ^. sqlValue
        item <- selectTable syndicatedItemsTable
        where_ $ item ^. sqlSynItSyndicate .== sqlInt4 (syn ^. synEntry . entryId)
        where_ $ item ^. sqlSynItUrl .== url
        pure (item ^. sqlSynItSequence, isStrict)
      forM_ mseq $ \(sq, isStrict) -> do
        mcnt :: Maybe Int64 <- rSelectOne $ countRows $ do
          item <- selectTable syndicatedItemsTable
          where_ $ item ^. sqlSynItSyndicate .== sqlInt4 (syn ^. synEntry . entryId)
          let op = if isStrict then (.>) else (.>=)
          where_ $ (item ^. sqlSynItSequence) `op` sqlInt4 sq
          pure $ item ^. sqlSynItSequence
        forM_ mcnt $ \cnt -> updateMetadata entry (M.singleton (mtdtSqlName AggregateCount) (toJSON cnt)) []
    Just "count-new" -> do
      mcnt :: Maybe Int64 <- rSelectOne $ countRows $ do
        item <- selectTable syndicatedItemsTable
        where_ $ item ^. sqlSynItSyndicate .== sqlInt4 (syn ^. synEntry . entryId)
        where_ $ isNull $ item ^. sqlSynItInstance
        where_ $ item ^. sqlSynItRead .== sqlBool False
        pure $ item ^. sqlSynItSequence
      forM_ mcnt $ \cnt -> updateMetadata entry (M.singleton (mtdtSqlName AggregateCount) (toJSON cnt)) []
    _ -> pure ()
  where
    sqlI = entry ^. entryId
