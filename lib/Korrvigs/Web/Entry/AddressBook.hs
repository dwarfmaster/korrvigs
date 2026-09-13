module Korrvigs.Web.Entry.AddressBook (content, embed) where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Web.Backend
import Yesod

embed :: Int -> AddressBook -> Handler Widget
embed _ abook = do
  pure
    [whamlet|
    <table>
      <tr>
        <td>Server
        <td>#{abook ^. abookServer}
      <tr>
        <td>User
        <td>#{abook ^. abookUser}
  |]

content :: AddressBook -> Handler Widget
content = embed 0
