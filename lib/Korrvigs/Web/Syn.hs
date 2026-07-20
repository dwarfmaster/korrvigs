module Korrvigs.Web.Syn where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Syndicate.Sync
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

getSynItemReadR :: WebId -> Int -> Handler TypedContent
getSynItemReadR (WId i) sq = do
  entry <- load i >>= maybe notFound pure
  syn <- maybe notFound pure $ entry ^? _Syndicate
  readItem syn sq
  pure $ toTypedContent ()
