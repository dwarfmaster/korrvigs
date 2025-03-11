module Korrvigs.Actions.Utils where

import Data.Profunctor.Product.Default
import Korrvigs.Entry
import Korrvigs.Monad
import Opaleye

genSqlLoad :: forall w sql hs kd m. (Default FromFields sql hs, Default Unpackspec sql sql, MonadKorrvigs m) => Table w sql -> (sql -> Field SqlText) -> (hs -> Entry -> kd) -> Id -> ((Entry -> kd) -> Entry) -> m (Maybe Entry)
genSqlLoad tbl getId fromRow i cstr = do
  sel <- rSelectOne $ do
    row <- selectTable tbl
    where_ $ getId row .== sqlId i
    pure row
  case (sel :: Maybe hs) of
    Nothing -> pure Nothing
    Just row -> pure $ Just $ cstr $ fromRow row
