module Korrvigs.Web.Entry.Calendar (content, embed) where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Web.Backend
import Yesod

embed :: Int -> Calendar -> Handler Widget
embed _ cal = do
  pure
    [whamlet|
    <table>
      <tr>
        <td>Server
        <td>#{cal ^. calServer}
      <tr>
        <td>User
        <td>#{cal ^. calUser}
      <tr>
        <td>Calendar
        <td>#{cal ^. calName}
  |]

content :: Calendar -> Handler Widget
content = embed 0
