module Korrvigs.Web.Vis.Timeline where

import Control.Lens hiding ((.=))
import Data.Aeson.Text (encodeToTextBuilder)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time.Calendar
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (a, preEscapedTextValue, toMarkup, (!))
import Text.Blaze.Html5.Attributes (href)
import Text.Julius
import Yesod

data Item = Item
  { _itemText :: Text,
    _itemStart :: ZonedTime,
    _itemEnd :: Maybe ZonedTime,
    _itemGroup :: Text,
    _itemTarget :: Maybe Text
  }

makeLenses ''Item

mkItemJS :: Item -> Value
mkItemJS item =
  object
    [ "content" .= content,
      "start" .= formatShow iso8601Format (item ^. itemStart),
      "end"
        .= if spanDay
          then maybe Null (toJSON . formatShow iso8601Format) (item ^. itemEnd)
          else Null,
      "group" .= (item ^. itemGroup)
    ]
  where
    content :: Text
    content = case item ^. itemTarget of
      Nothing -> item ^. itemText
      Just url -> LT.toStrict $ renderHtml $ a ! href (preEscapedTextValue url) $ toMarkup $ item ^. itemText
    zonedDay :: ZonedTime -> Day
    zonedDay = localDay . zonedTimeToLocalTime
    spanDay :: Bool
    spanDay = case item ^. itemEnd of
      Nothing -> False
      Just end -> zonedDay end /= zonedDay (item ^. itemStart)

mkGroupJS :: Text -> Value
mkGroupJS group =
  object
    [ "id" .= group,
      "content" .= group
    ]

timeline :: Text -> [Item] -> Handler Widget
timeline var items = do
  timeId <- newIdent
  let setup = rawJS $ "setup" <> var
  let groups = mkGroupJS <$> nub (view itemGroup <$> items)
  let groupsJS = rawJS $ encodeToTextBuilder groups
  let itemsJS = rawJS $ encodeToTextBuilder $ array $ mkItemJS <$> items
  pure $ do
    Rcs.visTimeline
    [whamlet|<div ##{timeId}>|]
    toWidget
      [cassius|
        ##{timeId}
          width: 100%
        .vis-text
          color: var(--base07) !important
        .vis-inner
          color: var(--base07)
        .vis-item
          border-width: 2px !important
          background-color: var(--base00) !important
          color: var(--base07) !important
        .vis-box
          border-color: var(--base0E) !important
        .vis-range
          border-color: var(--base0F) !important
      |]
    toWidget
      [julius|
      const #{setup} = function() {
        const div = document.getElementById(#{timeId});
        const items = #{itemsJS}
        const timeItems = items.map((item) => ({
          content: item.content,
          start: Date.parse(item.start),
          end: ((item.end == null) ? null : Date.parse(item.end)),
          group: item.group
        }));
        const groups = #{groupsJS}
        const options = {};
        var timeline = new vis.Timeline(div, timeItems, groups, options);
        return timeline
      }
      var #{rawJS var} = #{setup}()
    |]
