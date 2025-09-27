module Korrvigs.Web.Syn where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Syndicate.New
import Korrvigs.Syndicate.Sync
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Network.HTTP.Types.Status
import Yesod

getSynItemImportR :: WebId -> Int -> Handler TypedContent
getSynItemImportR (WId i) sq = do
  entry <- load i >>= maybe notFound pure
  syn <- maybe notFound pure $ entry ^? _Syndicate
  ni <- newFromItem syn sq
  redirectWith status308 $ EntryR $ WId ni

getSynItemReadR :: WebId -> Int -> Handler TypedContent
getSynItemReadR (WId i) sq = do
  entry <- load i >>= maybe notFound pure
  syn <- maybe notFound pure $ entry ^? _Syndicate
  readItem syn sq
  pure $ toTypedContent ()
