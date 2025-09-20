module Korrvigs.Web.Syn where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Syndicate.New
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Network.HTTP.Types.Status
import Yesod

getSynItemR :: WebId -> Int -> Handler TypedContent
getSynItemR (WId i) sq = do
  entry <- load i >>= maybe notFound pure
  syn <- maybe notFound pure $ entry ^? _Syndicate
  ni <- newFromItem syn sq
  redirectWith status308 $ EntryR $ WId ni
