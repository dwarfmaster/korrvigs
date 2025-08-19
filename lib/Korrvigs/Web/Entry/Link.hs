module Korrvigs.Web.Entry.Link (content, embed) where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

embed :: Int -> Link -> Handler Widget
embed lvl link = do
  let i = link ^. linkEntry . name
  abstract <- rSelectMtdt Abstract $ sqlId i
  rSelectMtdt Cover (sqlId i) >>= \case
    Just cid ->
      pure
        [whamlet|
      $maybe a <- abstract
        <p>
          #{a}
      <a href=#{ref}>
        <img src=@{EntryDownloadR $ WId $ MkId cid} style="width: 100%">
    |]
    Nothing ->
      pure
        [whamlet|
      <a href=#{ref}>#{ref}
      $maybe a <- abstract
        <p>
          #{a}
      $if lvl == 0
        <iframe width=100% height=700 src=#{ref}>
    |]
  where
    ref = link ^. linkRef

content :: Link -> Handler Widget
content = embed 0
