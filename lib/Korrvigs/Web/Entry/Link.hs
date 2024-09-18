module Korrvigs.Web.Entry.Link (content, embed) where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Web.Backend
import Yesod

embed :: Int -> Link -> Handler Widget
embed _ link =
  pure
    [whamlet|
    <a href=#{lnk}>#{link ^. linkRef}
  |]
  where
    lnk = link ^. linkProtocol <> "://" <> link ^. linkRef

content :: Link -> Handler Widget
content = embed 0
