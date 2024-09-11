module Korrvigs.Web.Entry.Link (content) where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Web.Backend
import Yesod

content :: Link -> Handler Widget
content link =
  pure
    [whamlet|
    <a href=#{lnk}>#{link ^. linkRef}
  |]
  where
    lnk = link ^. linkProtocol <> "://" <> link ^. linkRef
