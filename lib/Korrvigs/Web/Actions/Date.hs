module Korrvigs.Web.Actions.Date where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad.Metadata
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Yesod

dateTarget :: ActionTarget -> ActionCond
dateTarget (TargetEntry entry) | entry ^. kind /= Event = ActCondAlways
dateTarget _ = ActCondNever

dateForm :: AForm Handler (LocalTime, Maybe LocalTime)
dateForm =
  (,)
    <$> areq datetimeLocalField "Start" Nothing
    <*> aopt datetimeLocalField "End" Nothing

dateTitle :: ActionTarget -> Text
dateTitle = const "Set date"

runDate :: (LocalTime, Maybe LocalTime) -> ActionTarget -> Handler ActionReaction
runDate (start, _) (TargetEntry entry) = do
  tz <- liftIO getCurrentTimeZone
  let startDate = ZonedTime start tz
  -- TODO end date support
  -- let endDate = flip ZonedTime tz <$> end
  updateDate entry (Just startDate)
  pure $ def & reactMsg ?~ [shamlet|<p>Set date|]
runDate _ _ = pure def
