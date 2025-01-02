module Korrvigs.Web.Entry.Link (content, embed) where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Web.Backend
import Yesod

embed :: Int -> Link -> Handler Widget
embed lvl link =
  pure
    [whamlet|
    <a href=#{ref}>#{ref}
    $if lvl == 0
      <iframe width=100% height=700 src=#{ref}>
  |]
  where
    ref = link ^. linkRef

content :: Link -> Handler Widget
content = embed 0
